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
import Tree.Diff exposing (Diff(..), Tail(..))
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
    let
        keySub =
            if contentIsEditing model.content || cellIsEditing model.zipper then
                Sub.none

            else
                Events.onKeyPress (Decode.map (\x -> KeyDowns x model.zipper) keyDecoder)

        animationSub =
            if model.session.settings.animation then
                Animation.subscription Animate [ model.style ]

            else
                Sub.none
    in
    Sub.batch [ keySub, animationSub ]



-- INIT


init : String -> Repo -> Session -> ( Model, Cmd Msg )
init key repo session =
    let
        initModel =
            { session = session
            , key = key
            , repo = repo
            , zipper = Loading
            , content = Loading
            , log = NotAsked
            , notifications = []
            , hover = False
            , utc = Time.utc
            , showchanges = False
            , diff = Nothing
            , dagRenderStyle = AsTree
            , blockEdit = False
            , style = Animation.style [ Animation.opacity 0.0 ]
            }
    in
    ( initModel
    , Cmd.batch
        [ Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        , Task.attempt GotZone Time.here
        , fetchDAG session.url { cid = repo.tree, location = repo.location } repo.unsaved
        , contentRequest session.url { cid = repo.tree, location = repo.location } repo.unsaved

        --, case repo.head of
        --    Just head ->
        --        Task.attempt GotLog <| fetchHistory 10 session.url head []
        --    Nothing ->
        --        Cmd.none
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
    , diff : Maybe (Diff Node)
    , dagRenderStyle : DAGstyle
    , blockEdit : Bool
    , style : Animation.State
    }


type alias DAG =
    Zipper Node


type Action
    = Remove Link
    | Set Link
    | Move Link Int


type alias Styles =
    { open : List Animation.Property
    , closed : List Animation.Property
    }


type Notification
    = ServerError String
    | PinAdd String


type Msg
    = NoOp
    | Pin DAG
    | GotNewRootHash String Repo (Result Http.Error Hash)
    | GotUpdatedContentHash DAG (Result Http.Error Hash)
    | GotNodeContent (Result Http.Error Content)
    | AddText Content
    | ChangeFocus DAG Node
    | UpdateFocus DAG
    | DownloadAsJson String
    | Pick DAG Content
    | GotFiles DAG Content File (List File)
    | Perform DAG Action
    | DragEnter
    | DragLeave
    | AddSubTree DAG (Result Http.Error (Tree Node))
    | DiscardChanges
    | ClearNotifications
    | GotDAG (Result Http.Error DAG)
    | GotLog (Result Http.Error (List Commit))
    | Append DAG
    | RemoveFocus (Tree Node) DAG DAG
    | LinksHashReplaced (Result Http.Error DAG)
    | PassedSlowLoadThreshold
    | NodeChanged DAG (Result Http.Error Bool)
    | GotCommitHash String (Result Http.Error Hash)
    | GotZone (Result Http.Error Time.Zone)
    | KeyDowns String (Remote DAG)
    | PinDAG (Result Http.Error (List String))
    | FetchWholeDAG DAG
    | ShowChanges
    | DAGtoFileSystem DAG
    | NodeSaved DAG (Result Http.Error ())
    | RootHashRecursivelyPinned (Result Http.Error (List String))
    | GotNewContent DAG (Result Http.Error (List Link))
    | GotTree (Tree Node) (Result Http.Error (Tree Node))
    | DiffFocusedTree DAG
    | SetRenderStyle DAG DAGstyle
    | Animate Animation.Msg


type DAGstyle
    = AsTable
    | AsTree



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
        SetRenderStyle zipper style ->
            ( { model | dagRenderStyle = style }, Task.attempt (AddSubTree zipper) <| getTreeOnDepth url 2 (Zipper.tree zipper) )

        Animate animMsg ->
            ( { model | style = Animation.update animMsg model.style }, Cmd.none )

        RootHashRecursivelyPinned (Ok pins) ->
            ( { model | notifications = [ PinAdd <| "Успешно сохранено" ] }, Cmd.none )

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
            ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session, showchanges = False }
            , Cmd.batch
                [ Session.store <| Session.updateRepo model.key newRepo model.session
                , fetchDAG url currentPath Dict.empty
                ]
            )

        FetchWholeDAG dag ->
            ( model
            , Zipper.toTree dag
                |> fetchWholeDAG url repo.tree
                |> Task.andThen (Task.succeed << Zipper.fromTree)
                |> Task.attempt GotDAG
            )

        DiffFocusedTree dag ->
            ( model
            , fetchTree url { cid = repo.tree, location = .location <| Zipper.label dag } (oneLevelTree dag)
            )

        GotTree new_tree (Ok old_tree) ->
            ( { model | diff = Just <| Tree.Diff.diff old_tree new_tree }, Cmd.none )

        GotTree new_tree (Err _) ->
            ( model, Cmd.none )

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
            ( { model | zipper = Success dag, blockEdit = False }
            , Cmd.none
              --Content.fetchByCid url (.cid <| Zipper.label dag) GotNodeContent
            )

        GotDAG (Err error) ->
            ( { model
                | zipper = Failed error
                , blockEdit = False
                , repo = { repo | location = [] }
                , session = Session.updateRepo model.key { repo | location = [] } model.session
              }
            , Cmd.none
            )

        GotLog (Ok log) ->
            ( { model | log = Success log }, Cmd.none )

        GotLog (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки истории изменений" ] }, Cmd.none )

        RemoveFocus removed_tree ex_parent parent ->
            let
                diff =
                    Tree.Diff.diff (Zipper.tree parent) (Zipper.tree ex_parent)

                unsaved =
                    Tree.flatten removed_tree
                        |> List.foldl (Dict.remove << .location) model.repo.unsaved

                newRepo =
                    { repo | location = .location (Zipper.label ex_parent), unsaved = unsaved }
            in
            ( { model
                | zipper = Success ex_parent
                , repo = newRepo
                , diff = Just diff
                , session = Session.updateRepo model.key newRepo model.session
              }
            , updateLinksHash url ex_parent
            )

        Append zipper ->
            ( { model | zipper = Success zipper }, updateLinksHash url zipper )

        UpdateFocus zipper ->
            ( { model | zipper = Success zipper }
            , if Zipper.label zipper |> .editing then
                Cmd.none

              else
                checkNodeForChanges url repo.tree zipper
            )

        LinksHashReplaced (Ok zipper) ->
            let
                newRepo =
                    { repo | location = .location <| Zipper.label zipper }
            in
            ( { model | session = Session.updateRepo model.key newRepo model.session }
            , checkNodeForChanges url repo.tree zipper
            )

        LinksHashReplaced (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка при обновлении поля links у родителя (LinksHashReplaced) " ] }
            , Cmd.none
            )

        NodeChanged zipper (Ok changed) ->
            let
                node =
                    Zipper.label zipper

                unsaved_changes =
                    if changed then
                        Dict.insert node.location node model.repo.unsaved

                    else
                        Dict.remove node.location model.repo.unsaved

                newRepo =
                    { repo | unsaved = unsaved_changes }
            in
            ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session, blockEdit = changed }
            , Cmd.batch
                [ Session.store <| Session.updateRepo model.key newRepo model.session
                , case Zipper.parent zipper of
                    Just parent ->
                        updateLinksHash url parent

                    Nothing ->
                        Task.attempt GotDAG <|
                            Task.succeed <|
                                case setFocus repo.location zipper of
                                    Just focus ->
                                        focus

                                    Nothing ->
                                        zipper
                ]
            )

        NodeChanged _ (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса изменения узла (NodeChanged Msg)" ] }, Cmd.none )

        KeyDowns code (Success zipper) ->
            let
                node =
                    Zipper.label zipper
            in
            case String.toInt code of
                Just int ->
                    if node.color == int - 1 then
                        ( model, Cmd.none )

                    else
                        ( { model | zipper = Success (Zipper.replaceLabel { node | color = int - 1 } zipper) }
                        , checkNodeForChanges url model.repo.tree (Zipper.replaceLabel { node | color = int - 1 } zipper)
                        )

                --update (UpdateFocus <| Zipper.replaceLabel { node | color = int } zipper) model
                Nothing ->
                    ( model, Cmd.none )

        KeyDowns _ _ ->
            ( model, Cmd.none )

        GotUpdatedContentHash zipper (Ok cid) ->
            let
                node =
                    Zipper.label zipper
            in
            if cid == node.cid then
                let
                    newRepo =
                        { repo | unsaved = Dict.remove node.location model.repo.unsaved }
                in
                ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session }, Cmd.none )

            else
                ( model
                , Cmd.batch
                    [ checkNodeForChanges url repo.tree (Zipper.replaceLabel { node | cid = cid } zipper)
                    , Content.fetchByCid url cid GotNodeContent
                    ]
                )

        GotUpdatedContentHash _ (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса хэша файлов (GotUpdatedContentHash Msg)" ] }, Cmd.none )

        ChangeFocus zipper node ->
            let
                ( cmd, newZipper ) =
                    case setFocus node.location zipper of
                        Just focus ->
                            ( Task.attempt (AddSubTree focus) <|
                                case model.dagRenderStyle of
                                    AsTable ->
                                        getTreeOnDepth url 2 (Zipper.tree <| Zipper.replaceLabel node focus)

                                    AsTree ->
                                        getChildren url { node | expanded = True }
                            , Success focus
                              -- ^ label replaced for accept node editing status
                            )

                        Nothing ->
                            ( fetchDAG url { cid = repo.tree, location = node.location } model.repo.unsaved, Loading )
            in
            if node.location == model.repo.location then
                ( { model | zipper = Success <| Zipper.replaceLabel node zipper, showchanges = False }
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
                    [ Content.fetchByCid url node.cid GotNodeContent
                    , Session.store <| Session.updateRepo model.key { repo | location = node.location } model.session
                    , cmd
                    ]
                )

        ClearNotifications ->
            ( { model | notifications = [] }, Cmd.none )

        AddSubTree zipper (Ok tree) ->
            let
                newTree =
                    Tree.mapLabel (\x -> { x | expanded = False }) tree
            in
            ( { model | zipper = Success (Zipper.replaceTree newTree zipper) }, Cmd.none )

        AddSubTree _ (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки дочерних ячеек" ] }, Cmd.none )

        PinDAG (Ok nodes) ->
            ( { model
                | notifications = [ PinAdd <| "Репозиторий успешно закреплён в сети с адресом " ++ String.concat nodes ]
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

        Pick zipper content ->
            ( model, Content.pickFiles (GotFiles zipper content) )

        GotFiles zipper content file files ->
            let
                size =
                    List.map File.size (file :: files)
                        |> List.sum
                        |> (+) (contentSize content)
            in
            ( { model | hover = False }
            , Content.addFiles url content (file :: files) |> Task.attempt (GotNewContent zipper)
              --, Content.update url content (file :: files) |> Task.attempt (GotUpdatedContentHash { node | size = size })
            )

        GotNewContent zipper (Ok content) ->
            ( { model | content = Success content }, Content.getCid url content |> Task.attempt (GotUpdatedContentHash zipper) )

        GotNewContent zipper (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка загрузки файлов (GotNewContent Msg)" ] }, Cmd.none )

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
                    , fetchDAG url newPath model.repo.unsaved
                    , Content.fetch url newPath GotNodeContent
                    , Task.attempt RootHashRecursivelyPinned (pinRepoTree url repo.tree cid)
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
                , style = Animation.queue [ Animation.to [ Animation.opacity 1.0 ] ] model.style
              }
            , Cmd.none
            )

        GotNodeContent (Err _) ->
            ( { model | notifications = [ ServerError <| "Ошибка запроса файлов (GotNodeContent Msg) по адресу " ] }, Cmd.none )

        DownloadAsJson str ->
            ( model, File.Download.string "tree.json" "application/json" str )

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

        Pin zipper ->
            ( model
            , Cmd.batch
                [ Decode.at [ "Cid", "/" ] Decode.string
                    |> Api.task "POST" (Endpoint.dagPut url) (jsonToHttpBody <| ipldNodeEncoder <| Zipper.label zipper)
                    |> Task.attempt (GotNewRootHash model.key model.repo)
                , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
                ]
            )

        Perform zipper action ->
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
                                |> Task.attempt (GotUpdatedContentHash zipper)

                        _ ->
                            Cmd.none

                _ ->
                    Api.put url (contentEncoder links) (GotUpdatedContentHash zipper)
            )



--VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    let
        animation =
            if model.session.settings.animation then
                Animation.render model.style
                    |> List.map htmlAttribute

            else
                []
    in
    { title = "Suzdal Ontology Framework"
    , content =
        column
            ([ spacing 15
             , width fill
             , height fill
             , inFront (viewNotifications model.notifications)
             , inFront <|
                if model.blockEdit then
                    el
                        [ width fill
                        , height fill
                        , Background.color <| black 1.0
                        , alpha 0.3
                        , inFront (viewNotifications model.notifications)
                        ]
                        Loading.blockScreen

                else
                    none
             ]
                ++ animation
            )
            [ viewRemote (spinner "Загрузка дерева репозитория") (viewDAG model) model.zipper

            --, el
            --    [ alignBottom, width fill, padding 7, Background.color <| lightGrey 1.0, Font.size 12 ]
            --    (viewLog model.utc model.log)
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
            , Font.size 18

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
    if Dict.isEmpty changes then
        none

    else
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
            ]
            (text <| String.fromInt (Dict.size changes) ++ " изменений")


viewChanges : DAG -> Changes -> Element Msg
viewChanges zipper changes =
    column
        [ Background.color <| white 1.0
        , spacing 15
        , Font.size 12
        , padding 10
        , width fill
        , Border.color <| lightGrey 1.0
        , Border.width 1
        , Border.rounded 5
        ]
        [ el
            [ alignRight, Event.onClick DiscardChanges, Font.color <| orange 1.0, pointer ]
            (text "Отменить все изменения")
        , Dict.toList changes
            |> List.sortBy (List.length << Tuple.first)
            |> List.reverse
            |> List.map
                (\( k, node ) ->
                    column
                        [ width fill
                        , spacing 10
                        , mouseOver [ Background.color <| simpleColorCodeConverter node.color 0.2 ]
                        , padding 10
                        ]
                        [ viewCrumbs node zipper
                        , paragraph
                            [ Font.bold
                            , Font.size 18
                            , Event.onClick <| ChangeFocus zipper node
                            , pointer
                            , width fill
                            ]
                            [ text node.description ]
                        ]
                )
            |> column [ width fill ]
        ]


viewCrumbs : Node -> DAG -> Element Msg
viewCrumbs node zipper =
    case setFocus node.location zipper of
        Just focus ->
            getCrumbs focus []
                |> (\nodes -> List.drop (List.length nodes - 3) nodes)
                |> List.map (viewCrumbAsButton zipper)
                |> (\cells ->
                        if List.length (getCrumbs focus []) > 3 then
                            List.append [ el [ Font.bold ] <| text "..." ] cells

                        else
                            cells
                   )
                |> List.intersperse (text "/")
                |> row
                    [ spacing 7
                    , width shrink
                    , height shrink
                    , Font.size 12
                    , Font.italic
                    ]

        Nothing ->
            none


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
        , Event.onClick <| ChangeFocus dag node
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
            , inFront <| dot (not <| Repo.nodeChanged node.location changes) (black 0.8)
            ]
    in
    if
        --node is member of breadcrumbs list
        List.member node (getCrumbs dag [])
            -- focus location occur in-order and consecutively anywhere within the node location
            || isInfixOf (currentLocation dag) node.location
            -- and node is not in focus
            && node.location
            /= currentLocation dag
    then
        style 1.0
        -- if node in focus

    else if node.location == currentLocation dag then
        style 1.0 ++ [ Font.bold ]

    else
        style 0.3 ++ [ Font.color <| darkGrey 1.0 ]


viewDAG : Model -> DAG -> Element Msg
viewDAG model dag =
    let
        isCrumb node =
            el (cellStyle node model.repo.unsaved dag) <| viewCell model.session.settings.shownodeprops dag node

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
        , paddingEach { edges | top = 20, bottom = 20, left = 30 }
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
            ]
            [ viewMetaData model.repo
            , if model.showchanges && not (Dict.isEmpty model.repo.unsaved) then
                viewChanges dag model.repo.unsaved

              else
                column
                    [ width <| px 700
                    , spacing 15
                    , alignTop
                    , height fill
                    , scrollbarY
                    ]
                    [ case model.dagRenderStyle of
                        AsTree ->
                            viewDAGasTree dag []
                                |> List.map (row [ width fill, height fill, spacing 5 ] << List.map isCrumb)
                                |> column
                                    [ width fill
                                    , spacing 5
                                    ]

                        AsTable ->
                            viewDAGasTable dag
                    , viewNodeProps dag model.session.settings.shownodeprops
                    , if model.session.settings.shownodeprops then
                        row [] [ text "Общее количество ячеек в дереве: ", text <| String.fromInt <| countNodes dag ]

                      else
                        none
                    ]
            ]
        , column
            [ alignTop, spacing 10 ]
            [ case Maybe.map2 Tuple.pair (Zipper.removeTree dag) (Zipper.parent dag) of
                Just ( ex_parent, parent ) ->
                    RemoveFocus (Zipper.tree dag) (Zipper.replaceTree (Zipper.tree ex_parent |> indexChildren) ex_parent) parent
                        |> UI.Button.delete False

                Nothing ->
                    UI.Button.delete True NoOp
            , UI.Button.save (Dict.isEmpty model.repo.unsaved) (Dict.size model.repo.unsaved) (Pin <| Zipper.root dag)
            , UI.Button.addNode <| Append (appendChild dag)
            , UI.Button.download <| FetchWholeDAG dag
            , case model.dagRenderStyle of
                AsTree ->
                    UI.Button.renderDAGasTable <| SetRenderStyle dag AsTable

                AsTable ->
                    UI.Button.renderDAGasTree <| SetRenderStyle dag AsTree

            --, UI.Button.diffTree <| DiffFocusedTree dag
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
                [ viewFocusTitle <| Zipper.label dag
                , viewRemote none (viewContentEditor dag) model.content
                ]
            , viewRemote (spinner "Загрузка файлов") (viewContent dag model.session.url) model.content
            ]
        ]


viewContent : DAG -> Url -> Content -> Element Msg
viewContent dag url content =
    column
        [ scrollbarY, width fill, height fill, paddingEach { edges | right = 10 } ]
    <|
        List.map (viewLink url dag) content


viewContentEditor : DAG -> Content -> Element Msg
viewContentEditor zipper content =
    row
        [ alignRight, alignBottom ]
        [ UI.Button.addTextFile (AddText content)
        , UI.Button.addFile (Pick zipper content)

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


viewCell : Bool -> DAG -> Node -> Element Msg
viewCell shownodeprops dag node =
    if node.editing then
        Input.multiline
            [ height
                (fill
                    |> maximum 300
                    |> minimum 50
                )
            , width fill
            , Event.onLoseFocus <| UpdateFocus <| Zipper.replaceLabel { node | editing = False } dag
            , Font.center
            , centerX
            , centerY
            , htmlAttribute <| Html.Attributes.style "text-align" "inherit"
            , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
            ]
            { onChange = \new -> UpdateFocus <| Zipper.replaceLabel { node | description = new } dag
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
                    , color = darkGrey 1.0
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
            , Event.onClick <| ChangeFocus dag node
            , Event.onDoubleClick <| ChangeFocus dag { node | editing = True }
            , text (Route.locationToString "/" node.location)
                |> el [ Font.size 10, padding 3, transparent (not shownodeprops) ]
                |> inFront

            --<| Loading.dots node.expanded
            ]
        <|
            paragraph
                [ centerY
                , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
                , htmlAttribute <| Html.Attributes.style "text-overflow" "clip"
                ]
                [ text node.description ]


viewNodeProps : DAG -> Bool -> Element Msg
viewNodeProps zipper show =
    let
        node =
            Zipper.label zipper

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
                { onChange = \new -> UpdateFocus <| Zipper.replaceLabel { node | cid = new } zipper
                , text = node.cid
                , placeholder = Just <| Input.placeholder [] <| text "Идентификатор контента - хэш"
                , label = Input.labelAbove [] <| el [ Font.size 10 ] <| text "Адрес файлов"
                }
            , Input.text
                inputStyle
                { onChange = \new -> UpdateFocus <| Zipper.replaceLabel { node | links = new } zipper
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


viewLinkActions : Url -> DAG -> Link -> Element Msg
viewLinkActions url zipper link =
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
                { onPress = Just <| Perform zipper <| Set { link | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = link.name
                , url = Endpoint.file url link.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform zipper <| Move link -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform zipper <| Move link 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform zipper <| Remove link
                , label = text "x"
                }
            ]


viewLink : Url -> DAG -> Link -> Element Msg
viewLink url zipper link =
    let
        node =
            Zipper.label zipper

        style =
            case link.status of
                Selected ->
                    [ width fill
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| darkGrey 1.0
                    , Event.onDoubleClick <| Perform zipper <| Set { link | status = Editing }
                    , Event.onClick <| Perform zipper <| Set { link | status = Completed }
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
                    , Event.onDoubleClick <| Perform zipper <| Set { link | status = Editing }
                    , Event.onClick <| Perform zipper <| Set { link | status = Selected }
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| white 1.0
                    ]
    in
    column
        [ width fill ]
        [ viewLinkActions url zipper link
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
                        [ el [ width fill, padding 5, spacing 5, Font.italic, Font.center ] <| text link.name
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
                                    Perform zipper <| Remove link

                                else
                                    Perform zipper <| Set { link | status = Changed }
                             ]
                                ++ fontBy link.size
                            )
                            { onChange =
                                \new -> Perform zipper <| Set { link | description = new, size = String.length new }
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


viewDAGasTable : DAG -> Element Msg
viewDAGasTable zipper =
    let
        tree =
            Zipper.tree zipper

        node =
            Tree.label tree

        styled cell =
            el
                (cellStyle cell Dict.empty zipper ++ [ Background.color <| simpleColorCodeConverter cell.color 0.7 ])
            <|
                viewCell False zipper cell

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
                , htmlAttribute <| Html.Attributes.style "overflow-wrap" "inherit"
                ]
            <|
                el
                    [ Background.color <| white 1.0
                    , Font.bold
                    , width <| px 150
                    , height fill
                    , Font.color <| darkGrey 1.0 --<| simpleColorCodeConverter child_node.color 1.0
                    ]
                    (viewCell False zipper child_node)
                    :: List.map (styled << Tree.label) (Tree.children child)
    in
    column
        [ spacing 3
        , width fill
        , height fill
        ]
        [ viewCrumbs node zipper
        , el
            [ Font.size 20
            , Font.color <| darkGrey 1.0
            , width fill
            , height shrink
            ]
          <|
            viewCell False zipper node
        , Tree.children tree
            |> List.map childMap
            |> column
                [ width fill
                , height fill
                ]
        ]


viewNotifications : List Notification -> Element Msg
viewNotifications notifications =
    let
        style color =
            [ width fill
            , paddingXY 10 8
            , Font.size 14
            , Background.color <| color 1.0
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
        Just node ->
            Content.fetchByCid url node.cid GotNodeContent

        Nothing ->
            Content.fetch url path GotNodeContent


contentRequestIfDraft : Url -> Path -> Repo.Draft -> Cmd Msg
contentRequestIfDraft url path draft =
    case Maybe.andThen (setFocus path.location << Zipper.fromTree) draft of
        Just focus ->
            Content.fetchByCid url (.cid <| Zipper.label focus) GotNodeContent

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


fetchHead : Url -> Hash -> Task Http.Error (Maybe Hash)
fetchHead url hash =
    Api.task "GET" (Endpoint.dagGet url hash) Http.emptyBody Repo.commitDecoder
        |> Task.andThen (Task.succeed << .parent)


treeIsChanged : Url -> Repo -> Task Http.Error Bool
treeIsChanged url repo =
    case repo.head of
        Just head ->
            fetchHead url head
                |> Task.andThen (Task.succeed << Maybe.withDefault False << Maybe.map2 (/=) (Just repo.tree))

        Nothing ->
            Task.succeed False


fetchDAG : Url -> Path -> Changes -> Cmd Msg
fetchDAG url path changes =
    Api.task "GET" (Endpoint.dagGet url path.cid) Http.emptyBody nodeDecoder
        |> Task.andThen
            (\node ->
                fetchZipper url ({ path | location = [] } :: pathlist path) changes (Zipper.fromTree <| Tree.tree node [])
            )
        |> Task.attempt GotDAG


fetchZipper : Url -> List Path -> Changes -> DAG -> Task Http.Error DAG
fetchZipper url paths changes zipper =
    case paths of
        x :: xs ->
            case setFocus x.location zipper of
                Just focus ->
                    fetchChildren url focus changes
                        |> Task.andThen (fetchZipper url xs changes)

                Nothing ->
                    Task.fail (Http.BadUrl "Ошибка загрузки дерева")

        [] ->
            fetchChildren url zipper changes


fetchChildren : Url -> DAG -> Changes -> Task Http.Error DAG
fetchChildren url zipper changes =
    let
        node =
            Maybe.withDefault (Zipper.label zipper) (Dict.get (.location <| Zipper.label zipper) changes)
    in
    Decode.list nodeToTree
        |> Api.task "GET" (Endpoint.dagGet url node.links) Http.emptyBody
        |> Task.andThen (Task.succeed << indexChildren << Tree.tree node)
        |> Task.andThen (\tree -> Task.succeed <| Zipper.replaceTree tree zipper)


getChildren : Url -> Node -> Task Http.Error (Tree Node)
getChildren url node =
    Decode.list nodeToTree
        |> Api.task "GET" (Endpoint.dagGet url node.links) Http.emptyBody
        |> Task.andThen (Task.succeed << indexChildren << Tree.tree node)


getTreeOnDepth : Url -> Int -> Tree Node -> Task Http.Error (Tree Node)
getTreeOnDepth url depth tree =
    if depth == 0 then
        Task.succeed tree

    else
        Tree.label tree
            |> getChildren url
            |> Task.andThen (Task.sequence << List.map (getTreeOnDepth url (depth - 1)) << Tree.children)
            |> Task.andThen (\list -> Task.succeed <| Tree.replaceChildren list tree)


fetchWholeDAG : Url -> Hash -> Tree Node -> Task Http.Error (Tree Node)
fetchWholeDAG url cid tree =
    Api.task "GET" (Endpoint.dagGet url cid) Http.emptyBody nodeDecoder
        |> Task.andThen (getChildren url)
        |> Task.andThen (fetchWholeDAGhelp url << Zipper.fromTree)
        |> Task.andThen (Task.succeed << Zipper.toTree)


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


checkNodeForChanges : Url -> Hash -> DAG -> Cmd Msg
checkNodeForChanges url roothash zipper =
    let
        node =
            Zipper.label zipper
    in
    Api.task "GET" (Endpoint.node url { cid = roothash, location = node.location }) Http.emptyBody nodeDecoder
        |> Task.andThen (\x -> Task.succeed <| compareNodes x node)
        |> Task.onError (\_ -> Task.succeed True)
        |> Task.attempt (NodeChanged zipper)


childrenToHttpBody : DAG -> Http.Body
childrenToHttpBody z =
    Zipper.children z
        |> List.map Tree.label
        |> Encode.list ipldNodeEncoder
        |> jsonToHttpBody


updateLinksHash : Url -> DAG -> Cmd Msg
updateLinksHash url parent =
    Decode.at [ "Cid", "/" ] Decode.string
        |> Api.task "POST" (Endpoint.dagPut url) (childrenToHttpBody parent)
        |> Task.andThen
            (\hash ->
                let
                    label =
                        Zipper.label parent
                in
                Task.succeed <| Zipper.replaceLabel { label | links = hash } parent
            )
        |> Task.attempt LinksHashReplaced


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
            Failed (Http.BadUrl error)

        Just x ->
            Success x


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Nothing ->
            False

        Just _ ->
            True


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


fetchTree : Url -> Path -> Tree Node -> Cmd Msg
fetchTree url path tree =
    Task.map2
        (\node -> Tree.tree { node | location = .location <| Tree.label tree } << List.map Tree.singleton)
        (fetchTargetNode url path)
        (fetchTargetLinks url path)
        |> Task.andThen (Task.succeed << indexChildren)
        |> Task.attempt (GotTree tree)


fetchTargetNode : Url -> Path -> Task Http.Error Node
fetchTargetNode url path =
    Api.task "GET" (Endpoint.node url path) Http.emptyBody nodeDecoder


fetchTargetLinks : Url -> Path -> Task Http.Error (List Node)
fetchTargetLinks url path =
    Api.task "GET" (Endpoint.links url path) Http.emptyBody (Decode.list nodeDecoder)


pinRepoTree : Url -> Hash -> Hash -> Task Http.Error (List String)
pinRepoTree url old_hash new_hash =
    isPinned url old_hash
        |> Task.andThen
            (\pinned ->
                Api.task "GET"
                    (if pinned then
                        Endpoint.pinUpdate url old_hash new_hash

                     else
                        Endpoint.pinAdd url new_hash
                    )
                    Http.emptyBody
                    (Decode.field "Pins" <| Decode.list Decode.string)
            )


isPinned : Url -> Hash -> Task Http.Error Bool
isPinned url hash =
    Decode.field "Keys" (Decode.dict (Decode.field "Type" Decode.string))
        |> Api.task "GET" (Endpoint.pinLs url Nothing) Http.emptyBody
        |> Task.andThen (Task.succeed << Dict.member hash)



--pin : Url -> Hash -> Task Http.Error (List String)
--pin url hash =
--    Api.task "GET"
--        (Endpoint.pinAdd url hash)
--        Http.emptyBody
--        (Decode.field "Pins" <| Decode.list Decode.string)
--pinNode : Url -> Node -> Task Http.Error Node
--pinNode url node =
--    List.map (pin url) [ node.cid, node.links ]
--        |> Task.sequence
--        |> Task.andThen (\_ -> Task.succeed node)
--pinDAG : Url -> Node -> List Node -> Task Http.Error (List Node)
--pinDAG url node acc =
--    pinNode url node
--        |> Task.andThen (fetchChildren url)
--        |> Task.andThen
--            (\nodes ->
--                if List.isEmpty nodes then
--                    Task.succeed (node :: acc)
--                else
--                    List.map (\x -> pinDAG url x (node :: acc)) nodes
--                        |> Task.sequence
--                        |> Task.andThen (Task.succeed << List.concat)
--            )


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
                    red

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
--checkChildrenChanges : Url -> List Int -> List Node -> Changes -> List Node
--checkChildrenChanges url location nodes changes =
--    let
--        check location nodes changes acc =
--            case Dict.get (parent_location ++ List.length acc) changes of
--                Just maybe_node ->
--                    case maybe_node of
--                        Just node ->
--                            check parent_location (acc ++ node)
--                        Nothing ->
--                            acc
--                Nothing ->
--                    case List.filter ((==) (parent_location ++ List.length acc)) nodes of
--    in
--    List.map (replaceChanged changes) nodes
--        |> addedNodes location changes
--addedNodes : List Int -> Changes -> List Node -> List Node
--addedNodes location nodes changes =
--            case Dict.get location changes of
--                Just maybe_node ->
--                    case maybe_node of
--                        Just node ->
--                            addedNodes location (nodes ++ node)
--                        Nothing ->
--                            nodes
--                Nothing ->
--                    nodes


changesWithDiff : Diff Node -> Changes -> Changes
changesWithDiff diff changes =
    case diff of
        Copy label list_diff tail ->
            (case tail of
                Left list ->
                    --List.foldl (\x d -> Dict.remove (Tree.label x |> .location) d)
                    changes

                --list
                Right list ->
                    List.foldl (\x d -> Dict.insert (Tree.label x |> .location) (Tree.label x) d) changes list

                Empty ->
                    changes
            )
                |> Dict.union (List.foldl changesWithDiff changes list_diff)

        Replace old new ->
            Dict.insert (.location <| Tree.label new) (Tree.label new) changes

        Keep a ->
            Dict.remove (.location <| Tree.label a) changes


currentLocation : DAG -> List Int
currentLocation dag =
    .location (Zipper.label dag)


oneLevelTree : Zipper Node -> Tree Node
oneLevelTree zipper =
    Tree.tree (Zipper.label zipper) (List.map (\tree -> Tree.tree (Tree.label tree) []) <| Zipper.children zipper)



{-
   insertTree : Zipper Node -> Changes -> Changes
   insertTree zipper changes =
       Dict.insert (Zipper.label zipper |> .location) (oneLevelTree zipper) changes

-}


contentIsEditing : Remote (List { a | status : Status }) -> Bool
contentIsEditing remote_list =
    case remote_list of
        Success content ->
            List.any (\a -> Editing == a.status) content

        _ ->
            False


cellIsEditing : Remote DAG -> Bool
cellIsEditing remote_dag =
    case remote_dag of
        Success zipper ->
            .editing <| Zipper.label zipper

        _ ->
            False


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


viewDAGasTree : DAG -> List (List Node) -> List (List Node)
viewDAGasTree zipper acc =
    let
        appendChildren =
            (List.map Tree.label <| Zipper.children zipper) :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            viewDAGasTree parent appendChildren

        Nothing ->
            [ Zipper.label zipper ] :: appendChildren


viewRemote : Element Msg -> (a -> Element Msg) -> Remote a -> Element Msg
viewRemote loader viewer remotecontent =
    case remotecontent of
        Success content ->
            viewer content

        LoadingSlowly ->
            loader

        Failed error ->
            Loading.failed <|
                case error of
                    Http.BadUrl str ->
                        "Ошибка адреса ( " ++ str ++ " )"

                    Http.Timeout ->
                        "Истекло время ожидания"

                    Http.NetworkError ->
                        "Не получен ответ от IPFS, перезапустите приложение или включите IPFS"

                    Http.BadStatus i ->
                        "Код ответа сервера " ++ String.fromInt i

                    Http.BadBody str ->
                        "Некорректное тело запроса ( " ++ str ++ " )"

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
