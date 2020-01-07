module Page.Repo exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Animation
import Api exposing (Hash, jsonToHttpBody)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser.Dom as Dom
import Browser.Events as Events
import Content exposing (Content, Link, Status(..), addText, contentEncoder, contentSize)
import Dict
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import Filesize
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Loading exposing (spinner)
import MimeType as Mime
import Repo exposing (Changes, Commit, Node, Remote(..), Repo, ipldNodeEncoder, nodeDecoder)
import Result exposing (Result)
import Route exposing (Path)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UI.Button
import UI.Colors exposing (..)
import UI.Icons as Icons
import Url exposing (Url)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyPress (Decode.map (\x -> KeyDowns x model.zipper) keyDecoder)

        --, Animation.subscription Animate [ model.style ]
        ]



-- INIT


init : String -> Repo -> Session -> ( Model, Cmd Msg )
init key repo session =
    ( { session = session
      , key = key
      , repo = repo
      , zipper = Loading
      , content = Loading
      , log = NotAsked
      , notifications = []
      , hover = False
      , utc = Time.utc
      , showchanges = False

      --, style = Animation.style [ Animation.opacity 0.0 ]
      }
    , Cmd.batch
        [ Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        , fetchDAGwithNewNodes session.url { cid = repo.tree, location = repo.location } repo.unsaved
        , case repo.head of
            Just head ->
                Task.attempt GotLog <| fetchHistory 10 session.url head []

            Nothing ->
                Cmd.none
        , Task.attempt GotZone Time.here
        ]
    )



-- MODEL


type alias Model =
    { session : Session
    , notifications : List Notification
    , key : String
    , repo : Repo
    , zipper : Remote DAG
    , hover : Bool
    , content : Remote Content
    , log : Remote (List Commit)
    , utc : Time.Zone
    , showchanges : Bool

    --, style : Animation.State
    }


type alias DAG =
    Zipper Node


type Action
    = Remove Link
    | Set Link
    | Move Link Int


type alias Diff =
    { path : List Int
    , field : String
    , old : String
    , new : String
    }


type alias Styles =
    { open : List Animation.Property
    , closed : List Animation.Property
    }


type Notification
    = ServerError String
    | PinAdd String


type Msg
    = NoOp
    | DagPut DAG
    | GotNewRootHash String Repo (Result Http.Error Hash)
    | GotUpdatedContentHash Node (Result Http.Error Hash)
    | GotNodeContent (Result Http.Error Content)
    | AddText Content
    | ChangeFocus Node
    | UpdateFocus Node
    | DownloadAsJson String
    | Pick Node Content
    | GotFiles Node Content File (List File)
    | Perform Node Action
    | DragEnter
    | DragLeave
    | AddTree (List Int) (Result Http.Error (Tree Node))
    | DiscardChanges
    | ClearNotifications
    | GotDAG (Result Http.Error DAG)
    | GotLog (Result Http.Error (List Commit))
    | Append DAG
    | RemoveFocus DAG
    | UpdateParent DAG (Result Http.Error Node)
    | PassedSlowLoadThreshold
    | RecursivePin Node
    | NodeChanged Node (Result Http.Error Bool)
    | GotCommitHash String (Result Http.Error Hash)
    | GotZone (Result Http.Error Time.Zone)
    | KeyDowns String (Remote DAG)
    | PinDAG (Result Http.Error (List Node))
    | FetchWholeDAG
    | ShowChanges
    | DAGtoFileSystem DAG
    | NodeSaved DAG (Result Http.Error ())
    | RootHashRecursivelyPinned (Result Http.Error (List String))



--| Animate Animation.Msg
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        location =
            model.repo.location

        currentPath =
            { cid = model.repo.tree, location = location }

        repo =
            model.repo

        url =
            model.session.url
    in
    case msg of
        RootHashRecursivelyPinned (Ok pins) ->
            ( { model
                | notifications = [ PinAdd <| "Успешно сохранены " ++ String.join ", " pins ]
              }
            , Cmd.none
            )

        RootHashRecursivelyPinned (Err _) ->
            ( { model | notifications = [ ServerError "Не удалось сохранить" ] }, Cmd.none )

        DAGtoFileSystem dag ->
            ( model, nodeToFilestore url model.key dag )

        NodeSaved zipper (Ok _) ->
            ( model
            , case Zipper.forward zipper of
                Just next ->
                    nodeToFilestore url model.key next

                Nothing ->
                    Cmd.none
            )

        NodeSaved zipper (Err _) ->
            ( { model | notifications = [ ServerError "Не удалось сохранить" ] }, Cmd.none )

        ShowChanges ->
            ( { model | showchanges = not model.showchanges }, Cmd.none )

        DiscardChanges ->
            let
                newRepo =
                    { repo | unsaved = Dict.empty }
            in
            ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session }
            , Cmd.batch
                [ Session.store <| Session.updateRepo model.key newRepo model.session
                , fetchDAG url currentPath
                ]
            )

        FetchWholeDAG ->
            ( model, fetchWholeDAG url repo.tree )

        GotZone (Ok zone) ->
            ( { model | utc = zone }, Cmd.none )

        GotZone (Err _) ->
            ( { model | utc = Time.utc }, Cmd.none )

        GotCommitHash key (Ok hash) ->
            if key == model.key then
                ( { model
                    | repo = { repo | head = Just hash }
                    , session = Session.updateRepo key { repo | head = Just hash } model.session
                  }
                , Task.attempt GotLog <| fetchHistory 10 url hash []
                )

            else
                case Dict.get key model.session.repos of
                    Just r ->
                        ( { model | session = Session.updateRepo key { r | head = Just hash } model.session }, Cmd.none )

                    Nothing ->
                        ( { model | notifications = [ ServerError <| "Updated repo named " ++ key ++ " doesn't exist anymore" ] }
                        , Cmd.none
                        )

        GotCommitHash key (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка обновления истории хранилища" ++ key ] }, Cmd.none )

        GotDAG (Ok dag) ->
            ( { model | zipper = Success dag }, contentRequest url currentPath model.repo.unsaved )

        GotDAG (Err _) ->
            ( { model
                | notifications = [ ServerError "Не удалось загрузить искомую ячейку. Загружен корневой раздел" ]
                , repo = { repo | location = [] }
                , session = Session.updateRepo model.key { repo | location = [] } model.session
              }
            , fetchDAG url { cid = repo.tree, location = [] }
            )

        GotLog (Ok log) ->
            ( { model | log = Success log }, Cmd.none )

        GotLog (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки истории изменений" ] }, Cmd.none )

        RemoveFocus zipper ->
            case Zipper.removeTree zipper of
                Just parent ->
                    let
                        newRepo =
                            { repo
                                | location = .location (Zipper.label parent)
                                , unsaved =
                                    Tree.foldl (\label acc -> Dict.remove label.location acc) model.repo.unsaved (Zipper.tree zipper)
                            }
                    in
                    ( { model
                        | zipper = Success parent
                        , repo = newRepo
                        , session = Session.updateRepo model.key newRepo model.session
                      }
                    , updateParent (Endpoint.dagPut url) parent |> Task.attempt (UpdateParent parent)
                    )

                Nothing ->
                    ( { model | notifications = [ ServerError "Нечего больше удалять! (RemoveFocus Msg)" ] }, Cmd.none )

        UpdateParent parent (Ok node) ->
            ( { model
                | zipper = Success (Zipper.replaceLabel node parent)
                , repo = { repo | location = node.location }
                , session = Session.updateRepo model.key { repo | location = node.location } model.session
              }
            , checkNodeForChanges url repo.tree node model.repo.unsaved
            )

        UpdateParent _ (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка при обновлении поля links у родителя (UpdateParent) " ] }
            , Cmd.none
            )

        Append zipper ->
            let
                child =
                    newChild zipper

                unsaved =
                    Dict.insert child.location child model.repo.unsaved
            in
            ( { model
                | zipper = Success (appendChild zipper) --updateRemote appendChild model.zipper
                , repo = { repo | unsaved = unsaved }
                , session = Session.updateRepo model.key { repo | unsaved = unsaved } model.session
              }
            , Cmd.none
            )

        KeyDowns code (Success zipper) ->
            let
                node =
                    Zipper.label (keyAction code zipper)
            in
            ( { model | zipper = Success <| keyAction code zipper }
            , Cmd.none
            )

        KeyDowns _ _ ->
            ( model, Cmd.none )

        UpdateFocus node ->
            ( { model | zipper = updateRemoteZipper node.location (Zipper.replaceLabel node) model.zipper }
            , if node.editing then
                Cmd.none

              else
                checkNodeForChanges url repo.tree node model.repo.unsaved
            )

        NodeChanged node (Ok changed) ->
            let
                updateFun =
                    if changed then
                        Dict.insert node.location node

                    else
                        Dict.remove node.location

                newRepo =
                    { repo | unsaved = updateFun model.repo.unsaved }
            in
            ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session }
            , Cmd.batch
                [ --Content.fetchByCid url node.cid GotNodeContent
                  Session.store <| Session.updateRepo model.key newRepo model.session
                ]
            )

        NodeChanged _ (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса изменения узла (NodeChanged Msg)" ] }, Cmd.none )

        GotUpdatedContentHash node (Ok cid) ->
            if cid == node.cid then
                let
                    newRepo =
                        { repo | unsaved = Dict.remove node.location model.repo.unsaved }
                in
                ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session }, Cmd.none )

            else
                update (UpdateFocus { node | cid = cid }) model

        GotUpdatedContentHash node (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса хэша файлов (GotUpdatedContentHash Msg)" ] }, Cmd.none )

        ChangeFocus node ->
            let
                ( haveNoChildren, newZipper ) =
                    case model.zipper of
                        Success zipper ->
                            case setFocus node.location zipper of
                                Just focused_zipper ->
                                    ( List.isEmpty (Zipper.children focused_zipper)
                                    , Success <| Zipper.replaceLabel node focused_zipper
                                      -- ^ label replaced for accept node editing status
                                    )

                                Nothing ->
                                    ( False, model.zipper )

                        _ ->
                            ( False, model.zipper )
            in
            if node.location == model.repo.location then
                ( { model | zipper = newZipper, showchanges = False }
                , if node.editing then
                    Dom.focus (Route.locationToString "/" node.location)
                        |> Task.attempt (\_ -> NoOp)

                  else
                    Cmd.none
                )

            else
                ( { model
                    | zipper = newZipper
                    , repo = { repo | location = node.location }
                    , session = Session.updateRepo model.key { repo | location = node.location } model.session
                    , showchanges = False

                    --, style = Animation.interrupt [ Animation.set [ Animation.opacity 0.0 ] ] model.style
                  }
                , Cmd.batch
                    [ case Dict.get node.location model.repo.unsaved of
                        Just change ->
                            Content.fetchByCid url change.cid GotNodeContent

                        Nothing ->
                            Content.fetch url { currentPath | location = node.location } GotNodeContent
                    , Session.store <| Session.updateRepo model.key { repo | location = node.location } model.session
                    , if haveNoChildren then
                        getChildren url { node | expanded = True }
                            |> Task.attempt (AddTree node.location)

                      else
                        Cmd.none
                    ]
                )

        ClearNotifications ->
            ( { model | notifications = [] }, Cmd.none )

        AddTree to (Ok tree) ->
            let
                node =
                    Tree.label tree

                isNewChild k =
                    case List.Extra.init k of
                        Just x ->
                            x == node.location

                        Nothing ->
                            False

                newChildren : List Node
                newChildren =
                    Dict.filter (\k _ -> isNewChild k) model.repo.unsaved
                        |> Dict.values

                oldChildren : Changes
                oldChildren =
                    Tree.children tree
                        |> List.map Tree.label
                        |> List.map (\n -> Tuple.pair n.location n)
                        |> Dict.fromList

                treeWithNewNodes : List Node -> Dict.Dict (List Int) Node -> Dict.Dict (List Int) Node
                treeWithNewNodes children dict =
                    case children of
                        x :: xs ->
                            Dict.insert x.location x dict
                                |> treeWithNewNodes xs

                        [] ->
                            dict

                newTree =
                    treeWithNewNodes newChildren oldChildren
                        |> Dict.values
                        |> List.map Tree.singleton
                        |> Tree.tree { node | expanded = False }
            in
            ( { model | zipper = updateRemoteZipper node.location (Zipper.replaceTree newTree) model.zipper }, Cmd.none )

        AddTree _ (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки дочерних ячеек" ] }, Cmd.none )

        RecursivePin node ->
            ( model, pinDAG url node [] |> Task.attempt PinDAG )

        PinDAG (Ok nodes) ->
            ( { model
                | notifications = [ PinAdd <| "Успешно сохранено " ++ (String.fromInt <| List.length nodes) ++ " узлов" ]
              }
            , Cmd.none
            )

        PinDAG (Err _) ->
            ( { model | notifications = [ ServerError "Не удалось сохранить" ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        Pick node content ->
            ( model, Content.pickFiles (GotFiles node content) )

        GotFiles node content file files ->
            let
                size =
                    List.map File.size (file :: files)
                        |> List.sum
                        |> (+) (contentSize content)
            in
            ( { model | hover = False }
            , Content.update url content (file :: files) |> Task.attempt (GotUpdatedContentHash { node | size = size })
            )

        GotNewRootHash key upd_repo (Ok cid) ->
            let
                newPath =
                    { currentPath | cid = cid }

                newRepo =
                    { upd_repo | tree = cid, unsaved = Dict.empty }

                newSession =
                    Session.updateRepo key newRepo model.session
            in
            if key == model.key then
                ( { model | repo = newRepo, session = newSession }
                , Cmd.batch
                    [ Session.store newSession
                    , createCommit url model.session.settings.author upd_repo key <| "commit " ++ key
                    , fetchDAG url newPath
                    , Content.fetch url newPath GotNodeContent
                    , Task.attempt RootHashRecursivelyPinned (pin url cid)
                    ]
                )

            else
                ( { model | session = newSession }
                , Cmd.batch
                    [ Session.store newSession
                    , createCommit url model.session.settings.author upd_repo key <| "commit " ++ key
                    ]
                )

        GotNewRootHash key upd_repo (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка обновления репозитория (GotNewRootHash Msg)" ++ upd_repo.name ] }
            , Cmd.none
            )

        GotNodeContent (Ok nodes) ->
            ( { model
                | content = Success <| List.indexedMap (\i a -> { a | id = i }) nodes

                --, style = Animation.queue [ Animation.to [ Animation.opacity 1.0 ] ] model.style
              }
            , Cmd.none
            )

        GotNodeContent (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка запроса файлов (GotNodeContent Msg) по адресу " ] }, Cmd.none )

        DownloadAsJson str ->
            ( model, File.Download.string "136_structure.json" "application/json" str )

        AddText content ->
            ( { model | content = Success (Content.addText content) }
            , Task.attempt (\_ -> NoOp) <| Dom.focus "file-id-0"
            )

        PassedSlowLoadThreshold ->
            let
                loading remote =
                    case remote of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | content = loading model.content, zipper = loading model.zipper }, Cmd.none )

        DagPut zipper ->
            let
                rootbody =
                    jsonToHttpBody << ipldNodeEncoder << Zipper.label
            in
            ( { model | zipper = Loading }
            , Cmd.batch
                [ Dict.values model.repo.unsaved
                    |> List.reverse
                    |> commitChanges (Endpoint.dagPut url) zipper
                    |> Task.andThen
                        (\patched_zipper ->
                            Decode.at [ "Cid", "/" ] Decode.string
                                |> Api.task "POST" (Endpoint.dagPut url) (rootbody patched_zipper)
                        )
                    |> Task.attempt (GotNewRootHash model.key model.repo)
                , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
                ]
            )

        Perform node action ->
            let
                mapFun =
                    case action of
                        Remove link ->
                            List.Extra.removeAt link.id
                                >> List.indexedMap (\i a -> { a | id = i })

                        Move link offset ->
                            List.Extra.swapAt link.id (link.id + offset)
                                >> List.indexedMap (\i a -> { a | id = i })

                        Set link ->
                            List.Extra.setAt link.id link

                {- RemoveAll ->
                   List.filter (\a -> False)
                -}
                links =
                    case model.content of
                        Success content ->
                            mapFun content

                        _ ->
                            []
            in
            ( { model | content = Success links }
            , case action of
                Set link ->
                    case link.status of
                        Editing ->
                            Task.attempt (\_ -> NoOp) <| Dom.focus ("file-id-" ++ String.fromInt link.id)

                        Changed ->
                            let
                                body cid =
                                    List.Extra.setAt link.id { link | cid = cid } links
                                        |> contentEncoder
                                        |> jsonToHttpBody
                            in
                            Content.uploadText (Endpoint.add url) link.id link.description
                                |> Task.andThen
                                    (\x ->
                                        Api.task
                                            "POST"
                                            (Endpoint.dagPut url)
                                            (body x.cid)
                                            (Decode.at [ "Cid", "/" ] Decode.string)
                                    )
                                |> Task.attempt (GotUpdatedContentHash node)

                        _ ->
                            Cmd.none

                _ ->
                    Api.put url (contentEncoder links) (GotUpdatedContentHash node)
            )



{- Animate animMsg ->
   ( { model
       | style = Animation.update animMsg model.style
     }
   , Cmd.none
   )
-}
--VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    --let
    --     animation =
    --       Animation.render model.style
    --           |> List.map htmlAttribute
    --in
    { title = "Suzdal Ontology Framework"
    , content =
        column
            [ spacing 15
            , width fill
            , height fill
            , inFront (viewNotifications model.notifications)
            ]
            [ viewMetaData model.repo
            , viewRemote (spinner "Загрузка дерева репозитория") (viewDAG model) model.zipper
            , el
                [ alignBottom, width fill, padding 7, Background.color <| lightGrey 1.0, Font.size 12 ]
              <|
                viewLog model.utc model.log
            ]
    }


viewEmptyRepo : Repo -> Element Msg
viewEmptyRepo repo =
    el [] <| text "Этот репозиторий пуст, добавьте"


viewMetaData : Repo -> Element Msg
viewMetaData repo =
    row
        [ spacing 5
        , paddingXY 15 5
        , width fill
        , height shrink
        , Background.color <| lightGrey 0.2
        ]
        [ el
            [ padding 5
            , height shrink
            , centerY
            , Font.color <| darkGrey 1.0

            --, Event.onDoubleClick (UpdateQuery string)
            ]
          <|
            text repo.name
        , viewChangesLabel repo.unsaved
        , el [ padding 5, centerX, centerY, width <| px 30, alignRight ] <| html Icons.hash
        , el [ Font.color <| darkGrey 1.0 ] <| text repo.tree
        ]


viewChangesLabel : Changes -> Element Msg
viewChangesLabel changes =
    el
        [ padding 5
        , width shrink
        , Background.color <| orange 0.5
        , mouseOver [ Background.color <| lightGrey 1.0 ]
        , Border.rounded 5
        , Font.size 12
        , pointer
        , alignLeft
        , Event.onClick <| ShowChanges
        , transparent <| Dict.isEmpty changes
        ]
        (text <| String.fromInt (Dict.size changes) ++ " изменений")


viewChanges : DAG -> Changes -> Element Msg
viewChanges zipper changes =
    if Dict.isEmpty changes then
        none

    else
        column
            [ Background.color <| white 1.0, spacing 15, Font.size 12, padding 10, width fill, Border.width 1, Border.rounded 10 ]
            [ el [ alignRight, Event.onClick DiscardChanges, Font.color <| orange 1.0, pointer ] <| text "Отменить все изменения"
            , column
                [ width fill ]
              <|
                List.map
                    (\( _, v ) ->
                        column
                            [ width fill
                            , spacing 10
                            , mouseOver [ Background.color <| simpleColorCodeConverter v.color 0.2 ]
                            , padding 10
                            ]
                            [ viewCrumbs v zipper
                            , paragraph
                                [ Font.bold
                                , Font.size 18
                                , Event.onClick <| ChangeFocus v
                                , pointer
                                , width fill
                                ]
                                [ text v.description ]
                            ]
                    )
                <|
                    Dict.toList changes
            ]


viewCrumbs : Node -> DAG -> Element Msg
viewCrumbs node zipper =
    case setFocus node.location zipper of
        Just focus ->
            getCrumbs focus []
                |> (\nodes -> List.drop (List.length nodes - 3) nodes)
                |> List.map (viewCrumbAsButton zipper)
                |> List.append [ el [ Font.bold ] <| text "..." ]
                |> List.intersperse (text "/")
                |> wrappedRow
                    [ spacing 7
                    , Font.size 12
                    , Font.italic
                    ]

        Nothing ->
            text "Can't find this cell"


viewCrumbAsButton : DAG -> Node -> Element Msg
viewCrumbAsButton dag node =
    el
        [ width shrink
        , padding 5
        , Border.rounded 5
        , Border.width 1
        , if (.location <| Zipper.label dag) == node.location then
            Border.color <| darkGrey 1.0

          else
            Border.color <| lightGrey 0.5
        , mouseOver [ Background.color <| lightGrey 0.5 ]
        , Background.color <| simpleColorCodeConverter node.color 0.7
        , Font.color <| black 0.8
        , Event.onClick <| ChangeFocus node
        , pointer
        , htmlAttribute <| Html.Attributes.title node.description
        ]
    <|
        text <|
            String.left 30 node.description


cellStyle : Node -> Changes -> DAG -> List (Attribute Msg)
cellStyle node changes dag =
    let
        style alpha =
            [ width fill
            , height fill
            , Background.color <| simpleColorCodeConverter node.color alpha
            , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
            , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
            , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
            , inFront <| dot (isNotChanged node.location changes) (black 0.8)
            ]

        focus_location =
            .location <| Zipper.label dag
    in
    if
        --node is member of breadcrumbs list
        List.member node (getCrumbs dag [])
            -- focus location occur in-order and consecutively anywhere within the node location
            || isInfixOf focus_location node.location
            -- and node is not in focus
            && node.location
            /= focus_location
    then
        style 1.0
        -- if node in focus

    else if node.location == focus_location then
        style 1.0 ++ [ Font.bold ]

    else
        style 0.3 ++ [ Font.color <| darkGrey 1.0 ]


viewDAG : Model -> DAG -> Element Msg
viewDAG model dag =
    let
        isCrumb node =
            el (cellStyle node model.repo.unsaved dag) <| viewCell node

        focus =
            Zipper.label dag

        haveParent z =
            case Zipper.removeTree z of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    row
        [ width fill
        , height fill
        , spacing 20
        , paddingEach { edges | top = 10, bottom = 10, left = 30 }
        , htmlAttribute (Html.Attributes.style "flex-shrink" "1")
        , clip
        , Font.size 12
        ]
        [ column
            [ width <| px 700
            , spacing 15
            , alignTop
            , height fill
            , scrollbarY
            , inFront <|
                if model.showchanges then
                    viewChanges dag model.repo.unsaved

                else
                    none
            ]
            [ getContexts dag []
                |> List.map
                    (row [ width fill, height fill, spacing 5 ] << List.map isCrumb)
                |> column [ width fill, spacing 5 ]
            , viewNodeProps focus model.session.settings.shownodeprops
            , row [] [ text "Общее количество ячеек в дереве: ", text <| String.fromInt <| countNodes dag ]
            ]
        , column
            [ alignTop, spacing 10 ]
            [ UI.Button.delete (not <| haveParent dag) <| RemoveFocus dag
            , UI.Button.save (Dict.isEmpty model.repo.unsaved) (Dict.size model.repo.unsaved) (DagPut dag)
            , UI.Button.download <| RecursivePin <| Zipper.label dag
            , UI.Button.addNode <| Append dag
            ]
        , column
            [ width fill
            , height fill
            , paddingEach { edges | right = 28, left = 15 }
            , spacing 5
            ]
            [ row
                [ width fill
                , Border.color <| darkGrey 1.0
                , Border.widthEach { edges | bottom = 2 }
                ]
                [ viewFocusTitle focus
                , viewRemote none (viewContentEditor focus) model.content
                ]
            , viewRemote (spinner "Загрузка файлов") (viewContent dag model.session.url) model.content
            ]
        ]


viewContent : DAG -> Url -> Content -> Element Msg
viewContent dag url content =
    column
        [ scrollbarY, width fill, height fill, paddingEach { edges | right = 10 } ]
    <|
        List.map (viewLink url <| Zipper.label dag) content


viewContentEditor : Node -> Content -> Element Msg
viewContentEditor node content =
    row
        [ alignRight, alignBottom ]
        [ UI.Button.addTextFile (AddText content)
        , UI.Button.addFile (Pick node content)

        --, el [ alignRight, transparent (List.isEmpty content) ] <|
        --    button False Icons.trash2 <|
        --        Perform RemoveAll
        ]


viewFocusTitle : Node -> Element Msg
viewFocusTitle node =
    paragraph
        [ Font.size 24
        , width fill
        , paddingEach { edges | bottom = 5 }
        ]
        [ text node.description ]


viewLog : Time.Zone -> Remote (List Commit) -> Element Msg
viewLog zone remote =
    case remote of
        Success entries ->
            List.reverse entries
                |> List.take 1
                |> List.map (viewEntry zone)
                |> column
                    [ spacing 5
                    , width fill
                    ]

        _ ->
            none


viewEntry : Time.Zone -> Commit -> Element Msg
viewEntry zone commit =
    let
        viewField key value =
            row
                [ spacing 10 ]
                [ el [ alignRight, Font.italic, Font.color <| black 0.9 ] (text key)
                , text value
                ]
    in
    column
        [ spacing 5
        , paddingXY 7 0
        ]
        [ viewField "Автор изменения" <| String.join " " [ commit.author.name, "<", commit.author.email, ">" ]
        , viewField "Комментарий" commit.message
        , viewField "Адрес хранилища" commit.tree
        , viewField "Дата изменения" <| Timestamp.format zone commit.date
        , viewField "Адрес предыдущего изменения" <| Maybe.withDefault "none" commit.parent
        ]


dot : Bool -> Color -> Element Msg
dot pred color =
    el
        [ Background.color color
        , width <| px 2
        , height <| px 2
        , padding 3
        , alignTop
        , alignRight
        , moveDown 3.0
        , moveLeft 3.0
        , transparent pred
        , Border.rounded 10
        ]
        none


viewCrumbAsLink : Hash -> Node -> Element Msg
viewCrumbAsLink root node =
    link
        [ width shrink
        , padding 5
        , Border.rounded 10
        , Border.width 2
        , Border.color <| lightGrey 0.5
        , mouseOver [ Background.color <| lightGrey 0.5 ]
        ]
        { url = Route.pathToUrl { cid = root, location = node.location }
        , label = paragraph [] [ text node.description ]
        }


viewCell : Node -> Element Msg
viewCell node =
    if node.editing then
        Input.multiline
            [ height
                (fill
                    |> maximum 300
                    |> minimum 50
                )
            , width fill
            , Event.onLoseFocus <| UpdateFocus { node | editing = False }
            , Font.center
            , centerX
            , centerY
            , htmlAttribute <| Html.Attributes.style "text-align" "inherit"
            , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
            ]
            { onChange = \new -> UpdateFocus { node | description = new }
            , text = node.description
            , placeholder = Just <| Input.placeholder [] <| el [] none
            , label = Input.labelHidden "Data input"
            , spellcheck = True
            }

    else
        el
            [ mouseOver <|
                [ Border.shadow
                    { offset = ( 1, 1 )
                    , size = 1
                    , blur = 0
                    , color = lightGrey 1.0
                    }
                ]
            , width fill
            , height (fill |> minimum 40)
            , pointer
            , Font.center
            , centerX
            , centerY
            , padding 15
            , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
            , Event.onClick <| ChangeFocus node
            , Event.onDoubleClick <| ChangeFocus { node | editing = True }
            , inFront <| Loading.dots node.expanded
            ]
        <|
            paragraph
                [ centerY
                , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
                , htmlAttribute <| Html.Attributes.style "text-overflow" "clip"
                ]
                [ text node.description ]


viewNodeProps : Node -> Bool -> Element Msg
viewNodeProps node show =
    let
        inputStyle =
            [ width fill
            , spacing 5
            , padding 4
            , Background.color <| lightGrey 1.0
            , Border.widthEach { edges | bottom = 1 }
            , Border.color <| darkGrey 1.0
            , Border.rounded 0
            , alignRight
            , Font.bold
            ]
    in
    if show then
        column
            [ width fill
            , spacing 10
            , Background.color <| lightGrey 1.0
            , Border.rounded 5
            , padding 10
            ]
            [ Input.text
                inputStyle
                { onChange = \new -> UpdateFocus { node | cid = new }
                , text = node.cid
                , placeholder = Just <| Input.placeholder [] <| text "Идентификатор контента - хэш"
                , label = Input.labelAbove [] <| el [ Font.size 10 ] <| text "Адрес файлов"
                }
            , Input.text
                inputStyle
                { onChange = \new -> UpdateFocus { node | links = new }
                , text = node.links
                , placeholder = Just <| Input.placeholder [] <| text "Ссылки"
                , label = Input.labelAbove [] <| el [ Font.size 10 ] <| text "Адрес дочерних ячеек"
                }
            , row [ spacing 5 ]
                [ text "Адрес ячейки в дереве"
                , viewLocation node.location
                ]
            ]

    else
        none


viewLocation : List Int -> Element Msg
viewLocation list =
    row [ Font.bold, spacing 5 ] <|
        List.intersperse (text " / ") <|
            List.map (text << String.fromInt) list


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewLinkActions : Url -> Node -> Link -> Element Msg
viewLinkActions url node link =
    let
        actionStyle =
            [ mouseOver <| [ Background.color <| lightGrey 1.0 ]
            , paddingXY 7 2
            , Font.size 12
            , Border.rounded 3
            ]
    in
    el
        [ width fill ]
    <|
        row
            [ if link.status /= Selected then
                htmlAttribute <| Html.Attributes.style "visibility" "hidden"

              else
                alignRight
            , spacing 5
            , padding 5
            ]
            [ newTabLink
                actionStyle
                { url = Endpoint.file url link.cid
                , label = text "открыть"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Set { link | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = link.name
                , url = Endpoint.file url link.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Move link -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Move link 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Remove link
                , label = text "x"
                }
            ]


viewLink : Url -> Node -> Link -> Element Msg
viewLink url node link =
    let
        style =
            case link.status of
                Selected ->
                    [ width fill
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| darkGrey 1.0
                    , Event.onDoubleClick <| Perform node <| Set { link | status = Editing }
                    , Event.onClick <| Perform node <| Set { link | status = Completed }
                    ]

                Editing ->
                    [ width fill
                    , Background.color <| lightGrey 0.3
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| darkGrey 1.0
                    ]

                _ ->
                    [ width fill
                    , Event.onDoubleClick <| Perform node <| Set { link | status = Editing }
                    , Event.onClick <| Perform node <| Set { link | status = Selected }
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| white 1.0
                    ]
    in
    column
        [ width fill ]
        [ viewLinkActions url node link
        , el style <|
            case link.mimetype of
                Just (Mime.Video _) ->
                    column
                        [ width fill
                        , Font.color <| darkGrey 1.0
                        , mouseOver [ Font.color <| black 1.0 ]
                        ]
                        [ el [ width fill, padding 5, spacing 5, Font.italic ] <| text link.name
                        , html <|
                            Html.video
                                [ Html.Attributes.src <| Endpoint.file url link.cid
                                , Html.Attributes.controls True
                                , Html.Attributes.style "width" "100%"
                                ]
                                []
                        ]

                Just (Mime.Audio _) ->
                    column
                        [ width fill
                        , Font.color <| darkGrey 1.0
                        , mouseOver [ Font.color <| black 1.0 ]
                        ]
                        [ el [ width fill, padding 5, spacing 5, clip, Font.italic, Font.center ] <| text link.name
                        , html <|
                            Html.audio
                                [ Html.Attributes.src <| Endpoint.file url link.cid
                                , Html.Attributes.controls True
                                , Html.Attributes.style "width" "100%"
                                ]
                                []
                        ]

                Just (Mime.Image _) ->
                    image
                        [ width fill
                        , Font.color <| lightGrey 1.0
                        , mouseOver [ Font.color <| black 1.0 ]
                        ]
                        { src = Endpoint.file url link.cid
                        , description = link.name
                        }

                Just (Mime.Text Mime.PlainText) ->
                    if link.status == Editing then
                        Input.multiline
                            ([ height (shrink |> minimum 30)
                             , width fill
                             , padding 5
                             , spacing 5
                             , Border.widthEach edges
                             , Border.color <| white 1.0
                             , Border.rounded 0
                             , htmlAttribute <| Html.Attributes.id <| String.concat [ "file-id-", String.fromInt link.id ]
                             , Event.onLoseFocus <|
                                if String.isEmpty link.description then
                                    Perform node <| Remove link

                                else
                                    Perform node <| Set { link | status = Changed }
                             ]
                                ++ fontBy link.size
                            )
                            { onChange =
                                \new -> Perform node <| Set { link | description = new, size = String.length new }
                            , text = link.description
                            , placeholder = Just <| Input.placeholder [] <| el [] none
                            , label = Input.labelHidden "Text data input"
                            , spellcheck = True
                            }

                    else
                        paragraph
                            ([ width fill
                             , padding 5
                             , spacing 5
                             , clip
                             , mouseOver [ Background.color <| lightGrey 0.2 ]
                             ]
                                ++ fontBy link.size
                            )
                            [ text link.description ]

                _ ->
                    row
                        [ width fill
                        ]
                        [ el
                            [ padding 10
                            , width fill
                            , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                            , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
                            , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
                            ]
                          <|
                            text link.name
                        , el
                            [ Font.color <| darkGrey 1.0
                            , padding 5
                            , alignRight
                            , centerY
                            ]
                          <|
                            text <|
                                String.concat [ "(", Filesize.format link.size, ")" ]
                        ]
        ]


fontBy : Int -> List (E.Attribute Msg)
fontBy size =
    if size <= 140 then
        [ Font.size 24
        ]

    else if size > 140 && size <= 500 then
        [ Font.italic
        , Font.size 16
        ]

    else if size > 500 && size <= 1800 then
        [ Font.size 14 ]

    else
        []


viewTable : Path -> Tree Node -> Element Msg
viewTable path tree =
    let
        alpha =
            0.8
    in
    column
        [ height fill
        , width fill
        , spacing 5
        , alignTop
        ]
        [ viewCell (Tree.label tree)
            |> el
                [ Font.color <| darkGrey 1.0
                , width fill
                , height fill
                ]
        , Tree.children tree
            |> List.map (viewSector path)
            |> column
                [ width fill
                , height fill
                ]
        ]


viewSector : Path -> Tree Node -> Element Msg
viewSector path tree =
    let
        node =
            Tree.label tree

        childMap child =
            let
                child_node =
                    Tree.label child
            in
            row
                [ spacing 5
                , padding 5
                , width fill
                , height fill
                , Background.color <| lightGrey 0.5
                , htmlAttribute <| Html.Attributes.style "overflow-wrap" "inherit"
                ]
            <|
                [ viewCell child_node ]
                    ++ (List.map
                            (\x ->
                                if node.expanded then
                                    viewCell (Tree.label x)

                                else
                                    none
                            )
                        <|
                            Tree.children child
                       )
    in
    column
        [ spacing 3
        , width fill
        , height fill
        ]
        [ el
            [ Font.size 20
            , Font.color <| darkGrey 1.0
            , width fill
            , height shrink
            ]
          <|
            viewCell node
        , if node.expanded then
            column
                [ width fill
                , height fill
                ]
            <|
                List.map childMap <|
                    Tree.children tree

          else
            none
        ]


viewNotifications : List Notification -> Element Msg
viewNotifications notifications =
    let
        style color =
            [ width fill
            , paddingXY 10 8
            , Font.size 14
            , Background.color <| color 0.7
            , Border.width 1
            , Border.color <| color 1.0
            , Border.rounded 5
            ]

        render notification =
            case notification of
                ServerError str ->
                    row (style orange) [ text str, clearNotificationsButton ]

                PinAdd str ->
                    row (style green) [ text str, clearNotificationsButton ]
    in
    case notifications of
        [] ->
            none

        _ ->
            column [ width fill, padding 10 ] <| List.map render notifications


clearNotificationsButton : Element Msg
clearNotificationsButton =
    Input.button
        [ alignRight
        , height fill
        ]
        { onPress = Just ClearNotifications
        , label = el [ padding 3, Font.bold ] <| text "OK"
        }



-- REQUESTS


nodeToFilestore : Url -> String -> DAG -> Cmd Msg
nodeToFilestore url repo_key dag =
    let
        node =
            Zipper.label dag

        path =
            Repo.pathToNodeFolder repo_key node.location
    in
    Http.post
        { url = Endpoint.unwrap (Endpoint.filesWrite url <| path ++ "/node.json")
        , body = Api.jsonToHttpBody <| Repo.nodeEncoderForLocalStorage node
        , expect = Http.expectWhatever (NodeSaved dag)
        }


contentRequest : Url -> Path -> Changes -> Cmd Msg
contentRequest url path changes =
    case Dict.get path.location changes of
        Just changed ->
            Content.fetchByCid url changed.cid GotNodeContent

        Nothing ->
            Content.fetch url path GotNodeContent


fetchHistory : Int -> Url -> Hash -> List Commit -> Task Http.Error (List Commit)
fetchHistory i url hash history =
    if i /= 0 then
        fetchCommit url hash
            |> Task.andThen
                (\commit ->
                    case commit.parent of
                        Just parent ->
                            fetchHistory (i - 1) url parent (commit :: history)

                        Nothing ->
                            Task.succeed (commit :: history)
                )

    else
        Task.succeed history


fetchCommit : Url -> Hash -> Task Http.Error Commit
fetchCommit url hash =
    Api.task "GET" (Endpoint.dagGet url hash) Http.emptyBody Repo.commitDecoder


fetchDAG : Url -> Path -> Cmd Msg
fetchDAG url path =
    Api.task "GET" (Endpoint.dagGet url path.cid) Http.emptyBody nodeDecoder
        |> Task.andThen (getChildren url)
        |> Task.andThen (fetchZipper url (pathlist path) << Zipper.fromTree)
        |> Task.attempt GotDAG


fetchZipper : Url -> List Path -> DAG -> Task Http.Error DAG
fetchZipper url paths zipper =
    case paths of
        x :: xs ->
            case setFocus x.location zipper of
                Just focus ->
                    fetchZipper url xs focus

                Nothing ->
                    Zipper.label zipper
                        |> getChildren url
                        |> Task.andThen (\tree -> Zipper.replaceTree tree zipper |> fetchZipper url (x :: xs))

        --Task.fail (Http.BadUrl <| Route.locationToString "/" x.location)
        [] ->
            Task.succeed zipper


fetchWholeDAG : Url -> Hash -> Cmd Msg
fetchWholeDAG url cid =
    Api.task "GET" (Endpoint.dagGet url cid) Http.emptyBody nodeDecoder
        |> Task.andThen (getChildren url)
        |> Task.andThen (fetchWholeDAGhelp url << Zipper.fromTree)
        |> Task.attempt GotDAG


fetchWholeDAGhelp : Url -> DAG -> Task Http.Error DAG
fetchWholeDAGhelp url zipper =
    case Zipper.forward zipper of
        Just next ->
            Zipper.label next
                |> getChildren url
                |> Task.andThen (\tree -> Zipper.replaceTree tree next |> fetchWholeDAGhelp url)

        Nothing ->
            Zipper.root zipper
                |> Task.succeed


fetchDAGwithNewNodes : Url -> Path -> Changes -> Cmd Msg
fetchDAGwithNewNodes url path changes =
    Api.task "GET" (Endpoint.dagGet url path.cid) Http.emptyBody nodeDecoder
        |> Task.andThen (getChildren url)
        |> Task.andThen (addNewNodesTask url path.cid (Dict.toList changes) << Zipper.fromTree)
        |> Task.andThen
            (\zipper ->
                case setFocus path.location zipper of
                    Just focus ->
                        Task.succeed focus

                    Nothing ->
                        fetchZipper url (pathlist path) zipper
            )
        |> Task.attempt GotDAG


addNewNodesTask : Url -> Hash -> List ( List Int, Node ) -> DAG -> Task Http.Error DAG
addNewNodesTask url cid changes zipper =
    case changes of
        ( location, node ) :: xs ->
            addNewNode url node (pathlist { cid = cid, location = location }) zipper
                |> Task.andThen (addNewNodesTask url cid xs)

        [] ->
            Task.succeed <| Zipper.root zipper


addNewNode : Url -> Node -> List Path -> DAG -> Task Http.Error DAG
addNewNode url node paths zipper =
    case paths of
        x :: xs ->
            case setFocus x.location zipper of
                Just focus ->
                    addNewNode url node xs focus

                Nothing ->
                    Zipper.label zipper
                        --|> Debug.log "FOCUS FAILED, fetch CHILDREN"
                        |> getChildren url
                        |> Task.andThen
                            (\tree ->
                                Zipper.replaceTree
                                    (if List.isEmpty (Tree.children tree) then
                                        Tree.appendChild (Tree.tree node []) (Zipper.tree zipper)

                                     else
                                        tree
                                    )
                                    zipper
                                    |> addNewNode url node (x :: xs)
                            )

        [] ->
            getChildren url node
                |> Task.andThen (\tree -> Task.succeed <| Zipper.replaceTree (Tree.replaceLabel node tree) zipper)


commitChanges : Endpoint -> DAG -> List Node -> Task Http.Error DAG
commitChanges endpoint zipper list =
    case list of
        x :: xs ->
            case Zipper.findFromRoot (\node -> node.location == x.location) zipper of
                Just z ->
                    commitChange endpoint z
                        |> Task.andThen (\focus -> commitChanges endpoint focus xs)

                Nothing ->
                    commitChanges endpoint zipper xs

        [] ->
            Task.succeed zipper


commitChange : Endpoint -> DAG -> Task Http.Error DAG
commitChange endpoint zipper =
    case Zipper.parent zipper of
        Just parent ->
            Decode.at [ "Cid", "/" ] Decode.string
                |> Api.task "POST" endpoint (childrenToHttpBody parent)
                |> Task.andThen
                    (\hash ->
                        let
                            label =
                                Zipper.label parent
                        in
                        Zipper.replaceLabel { label | links = hash } parent
                            |> commitChange endpoint
                    )

        Nothing ->
            Task.succeed zipper


checkNodeForChanges : Url -> Hash -> Node -> Changes -> Cmd Msg
checkNodeForChanges url roothash node changes =
    --case Dict.get node.location changes of
    --    Just changed_node ->
    --        compareNodes node changed_node
    --            |> Task.succeed
    --            |> Task.attempt (NodeChanged node)
    --    Nothing ->
    Api.task "GET" (Endpoint.node url { cid = roothash, location = node.location }) Http.emptyBody nodeDecoder
        |> Task.andThen (Task.succeed << compareNodes node)
        |> Task.attempt (NodeChanged node)


childrenToHttpBody : DAG -> Http.Body
childrenToHttpBody z =
    Zipper.children z
        |> List.map Tree.label
        |> Encode.list ipldNodeEncoder
        |> jsonToHttpBody


updateParent : Endpoint -> DAG -> Task Http.Error Node
updateParent endpoint zipper =
    Decode.at [ "Cid", "/" ] Decode.string
        |> Api.task "POST" endpoint (childrenToHttpBody zipper)
        |> Task.andThen
            (\hash ->
                let
                    label =
                        Zipper.label zipper
                in
                Task.succeed { label | links = hash }
            )


toMaybe : Remote a -> Maybe a
toMaybe =
    mapRemote Just >> withDefault Nothing


withDefault : a -> Remote a -> a
withDefault default data =
    case data of
        Success x ->
            x

        _ ->
            default


mapRemote : (a -> b) -> Remote a -> Remote b
mapRemote f data =
    case data of
        Success value ->
            Success (f value)

        Loading ->
            Loading

        NotAsked ->
            NotAsked

        Failed error ->
            Failed error

        LoadingSlowly ->
            LoadingSlowly


fromMaybe : String -> Maybe a -> Remote a
fromMaybe error maybe =
    case maybe of
        Nothing ->
            Failed error

        Just x ->
            Success x


updateRemoteZipper : List Int -> (DAG -> DAG) -> Remote DAG -> Remote DAG
updateRemoteZipper location fun z =
    toMaybe z
        |> Maybe.andThen (setFocus location)
        |> Maybe.andThen (Just << fun)
        |> Maybe.andThen (setFocus location)
        |> fromMaybe "couldn't set focus"


setFocus : List Int -> DAG -> Maybe DAG
setFocus location zipper =
    Zipper.findFromRoot (\x -> x.location == location) zipper


getChildren : Url -> Node -> Task Http.Error (Tree Node)
getChildren url node =
    Decode.list nodeToTree
        |> Api.task "GET" (Endpoint.dagGet url node.links) Http.emptyBody
        |> Task.andThen (\list -> Task.succeed <| indexChildren <| Tree.tree node list)


getChildrenNew : Url -> Node -> String -> Task Http.Error (Tree Node)
getChildrenNew url node repo_key =
    Repo.fetchChildren url node repo_key
        |> Task.andThen (Task.succeed << Tree.tree node << List.map Tree.singleton)


getTreeOnDepth : Url -> String -> Int -> Tree Node -> Task Http.Error (Tree Node)
getTreeOnDepth url root depth tree =
    if depth == 0 then
        Task.succeed tree

    else
        Tree.label tree
            |> getChildren url
            |> Task.andThen (Task.sequence << List.map (getTreeOnDepth url root (depth - 1)) << Tree.children)
            |> Task.andThen (\list -> Task.succeed <| Tree.replaceChildren list tree)


pin : Url -> Hash -> Task Http.Error (List String)
pin url hash =
    Api.task "GET"
        (Endpoint.pinAdd url hash)
        Http.emptyBody
        (Decode.field "Pins" <| Decode.list Decode.string)


pinNode : Url -> Node -> Task Http.Error Node
pinNode url node =
    List.map (pin url) [ node.cid, node.links ]
        |> Task.sequence
        |> Task.andThen (\_ -> Task.succeed node)


pinDAG : Url -> Node -> List Node -> Task Http.Error (List Node)
pinDAG url node acc =
    pinNode url node
        |> Task.andThen (fetchChildren url)
        |> Task.andThen
            (\nodes ->
                if List.isEmpty nodes then
                    Task.succeed (node :: acc)

                else
                    List.map (\x -> pinDAG url x (node :: acc)) nodes
                        |> Task.sequence
                        |> Task.andThen (Task.succeed << List.concat)
            )


fetchChildren : Url -> Node -> Task Http.Error (List Node)
fetchChildren url node =
    Api.task "GET" (Endpoint.dagGet url node.links) Http.emptyBody (Decode.list nodeDecoder)


createCommit : Url -> Repo.Author -> Repo -> String -> String -> Cmd Msg
createCommit url author repo key comment =
    let
        commit date =
            { author = author
            , date = date
            , message = comment
            , parent = repo.head
            , tree = repo.tree
            }

        body =
            Api.jsonToHttpBody << Repo.commitEncoder << commit
    in
    Time.now
        |> Task.andThen (\posix -> Api.task "PUT" (Endpoint.dagPut url) (body posix) Api.cidDecoder)
        |> Task.attempt (GotCommitHash key)


treeEncoder : Tree Node -> Encode.Value
treeEncoder tree =
    let
        node =
            Tree.label tree
    in
    Encode.object <|
        [ ( "description", Encode.string node.description )
        , ( "size", Encode.int node.size )
        , ( "cid"
          , Encode.object
                [ ( "/"
                  , Encode.string <|
                        if String.isEmpty node.cid then
                            "zdpuB2CQLxyv4vfsFPL17HYXtVrJjxRsAwXgP787w94hi7Gsk"

                        else
                            node.cid
                  )
                ]
          )
        , ( "name", Encode.string node.name )
        , ( "links", Encode.list treeEncoder <| Tree.children tree )
        , ( "color", Encode.int node.color )
        ]



-- DECODERS


nodeToTree : Decode.Decoder (Tree Node)
nodeToTree =
    Decode.map2 Tree.tree nodeDecoder (Decode.succeed [])



-- decode links field as ipld-link


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


colorCodeConverter : Int -> Float -> Float -> Color
colorCodeConverter i hue alpha =
    fromHsl (degrees <| hue + 60 * toFloat i) 0.92 0.5 alpha


simpleColorCodeConverter : Int -> Float -> Color
simpleColorCodeConverter i alpha =
    let
        color =
            case i of
                0 ->
                    yellow

                1 ->
                    orange

                2 ->
                    violet

                3 ->
                    blue

                4 ->
                    cyan

                5 ->
                    green

                6 ->
                    lightGrey

                7 ->
                    darkGrey

                8 ->
                    black

                9 ->
                    white

                _ ->
                    white
    in
    color alpha



-- HELPERS


countNodes : Zipper Node -> Int
countNodes zipper =
    Zipper.toTree zipper |> Tree.flatten |> List.length


addNewNodes : List ( List Int, Node ) -> DAG -> DAG
addNewNodes changes dag =
    case changes of
        ( location, node ) :: xs ->
            case setFocus location dag of
                Just newdag ->
                    Zipper.replaceLabel node newdag
                        |> addNewNodes xs

                Nothing ->
                    case List.Extra.init location |> Maybe.andThen (\x -> setFocus x dag) of
                        Just parent ->
                            Zipper.replaceTree (Zipper.tree parent |> Tree.appendChild (Tree.tree node [])) parent
                                |> addNewNodes xs

                        Nothing ->
                            addNewNodes xs dag

        [] ->
            dag


compareNodes : Node -> Node -> Bool
compareNodes new old =
    (old.description /= new.description)
        || (old.links /= new.links)
        || (old.cid /= new.cid)
        || (old.color /= new.color)


isNotChanged : List Int -> Changes -> Bool
isNotChanged location unsaved_changes =
    not (Dict.member location unsaved_changes)


isInZipper : List Int -> List (List Int) -> Bool
isInZipper loc loclist =
    case loc of
        x :: xs ->
            List.member xs loclist

        [] ->
            False


pathlist : Path -> List Path
pathlist path =
    processPath { path | location = List.reverse path.location } identity []



-- helper function - convert single path [0,1,2] to list of paths [[0], [0,1], [0,1,2]] for zipper render


processPath : Path -> (Path -> a) -> List a -> List a
processPath path fun acc =
    case path.location of
        x :: xs ->
            processPath
                { path | location = xs }
                fun
                (fun { path | location = List.reverse (x :: xs) } :: acc)

        [] ->
            acc


expandFocus : Tree Node -> Tree Node
expandFocus =
    Tree.mapLabel (\label -> { label | expanded = not label.expanded })


addPathsToTree : DAG -> DAG
addPathsToTree zipper =
    case Zipper.forward zipper of
        Just x ->
            Zipper.mapTree indexChildren x
                |> addPathsToTree

        Nothing ->
            zipper


indexChildren : Tree Node -> Tree Node
indexChildren tree =
    let
        indexed location =
            List.indexedMap
                (\idx val ->
                    let
                        node =
                            Tree.label val
                    in
                    Tree.replaceLabel
                        { node | location = location ++ [ idx ] }
                        val
                )
    in
    Tree.mapChildren (indexed <| .location <| Tree.label tree) tree


newChild : DAG -> Node
newChild zipper =
    let
        label =
            Zipper.label zipper

        location =
            Zipper.children zipper
                |> List.length
                |> List.singleton
                |> List.append label.location
    in
    { id = 0
    , editing = False
    , name = "0"
    , cid = "zdpuAtQy7GSHNcZxdBfmtowdL1d2WAFjJBwb6WAEfFJ6T4Gbi" --empty content list
    , links = "zdpuAtQy7GSHNcZxdBfmtowdL1d2WAFjJBwb6WAEfFJ6T4Gbi" --empty links list
    , size = 0
    , description = ""
    , color = 0
    , location = location
    , expanded = False
    }


appendChild : DAG -> DAG
appendChild zipper =
    let
        tree =
            Zipper.tree zipper
                |> Tree.appendChild (Tree.tree (newChild zipper) [])
    in
    Zipper.replaceTree tree zipper


getCrumbs : DAG -> List Node -> List Node
getCrumbs zipper acc =
    case Zipper.parent zipper of
        Just parent ->
            getCrumbs parent (Zipper.label parent :: acc)

        Nothing ->
            acc


getContexts : DAG -> List (List Node) -> List (List Node)
getContexts zipper acc =
    let
        appendChildren =
            (List.map Tree.label <| Zipper.children zipper) :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            getContexts parent appendChildren

        Nothing ->
            [ Zipper.label zipper ] :: appendChildren


keyAction : String -> DAG -> DAG
keyAction code zipper =
    let
        label =
            Zipper.label zipper

        function =
            case code of
                "a" ->
                    Zipper.previousSibling

                "d" ->
                    Zipper.nextSibling

                "w" ->
                    Zipper.parent

                "s" ->
                    Zipper.firstChild

                "F2" ->
                    \_ -> Just <| Zipper.replaceLabel { label | editing = True } zipper

                "Escape" ->
                    \_ -> Just <| Zipper.replaceLabel { label | editing = False } zipper

                _ ->
                    \_ -> Just zipper
    in
    case String.toInt code of
        Just int ->
            Zipper.replaceLabel { label | color = int } zipper

        Nothing ->
            zipper


viewRemote : Element Msg -> (a -> Element Msg) -> Remote a -> Element Msg
viewRemote loader viewer remotecontent =
    case remotecontent of
        Success content ->
            viewer content

        LoadingSlowly ->
            loader

        Failed message ->
            Loading.failed message

        _ ->
            none


{-| Return True if all the elements of the first list occur in-order and
consecutively anywhere within the second.

    isInfixOf [ 5, 7, 11 ] [ 2, 3, 5, 7, 11, 13 ]
    --> True

    isInfixOf [ 5, 7, 13 ] [ 2, 3, 5, 7, 11, 13 ]
    --> False

    isInfixOf [ 3, 5, 2 ] [ 2, 3, 5, 7, 11, 13 ]
    --> False

-}
isInfixOf : List a -> List a -> Bool
isInfixOf infixList list =
    case infixList of
        [] ->
            True

        x :: xs ->
            isInfixOfHelp x xs list


isInfixOfHelp : a -> List a -> List a -> Bool
isInfixOfHelp infixHead infixTail list =
    case list of
        [] ->
            False

        x :: xs ->
            if x == infixHead then
                isPrefixOf infixTail xs

            else
                isInfixOfHelp infixHead infixTail xs


{-| Take two lists and return `True`, if the first list is the prefix of the second list.
-}
isPrefixOf : List a -> List a -> Bool
isPrefixOf prefix list =
    case ( prefix, list ) of
        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( p :: ps, x :: xs ) ->
            p == x && isPrefixOf ps xs
