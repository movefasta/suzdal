module Page.Repo exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Animation
import Api exposing (Hash, jsonToHttpBody)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser.Dom as Dom
import Browser.Events as Events
import Content exposing (Content, Link, Status(..), addText, contentEncoder, contentSize)
import Dict exposing (Dict)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import File exposing (File)
import File.Download
import Filesize
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra
import Loading exposing (spinner)
import MimeType as Mime
import Page
import Process
import Repo exposing (Changes, Commit, Node, Repo, ipldNodeEncoder, nodeDecoder)
import Result exposing (Result)
import Route exposing (Path, Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UI.Button exposing (button)
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

      --, style = Animation.style [ Animation.opacity 0.0 ]
      }
    , Cmd.batch
        [ Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        , fetchDAG session.url { cid = repo.tree, location = repo.location }
        , case repo.head of
            Just head ->
                Task.attempt GotLog <| fetchHistory 10 session.url head []

            Nothing ->
                Cmd.none
        , Task.attempt GotZone Time.here

        --, Process.sleep 0 |> Task.perform (\_ -> RetrieveLocalStorageRepos)
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

    --, style : Animation.State
    }


type alias DAG =
    Zipper Node


type Remote a
    = NotAsked
    | Loading
    | LoadingSlowly
    | Success a
    | Failed


type Action
    = Remove Link
    | Set Link
    | Move Link Int
    | RemoveAll


type alias Entry =
    { time : Time.Posix
    , zone : Time.Zone
    , diff : Diff
    }


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
    | GotNewRootHash (Result Http.Error Hash)
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
    | GotSession Session.Session
    | AddTree (List Int) (Result Http.Error (Tree Node))
    | ClearNotifications
    | AddBookmark Path String
    | GotDAG (Result Http.Error DAG)
    | GotLog (Result Http.Error (List Commit))
    | Append DAG
    | RemoveFocus DAG
    | UpdateParent DAG (Result Http.Error Node)
    | PassedSlowLoadThreshold
    | RecursivePin Node
    | PinAddResponse Hash (Result Http.Error (List String))
    | NodeChanged Node (Result Http.Error Bool)
    | GotCommitHash (Result Http.Error Hash)
    | GotZone (Result Http.Error Time.Zone)
    | KeyDowns String (Remote DAG)



--| UpdateContent Content (Result Http.Error Content)
--| RetrieveLocalStorageRepos
--| GotRepo (Result Decode.Error Repo)
--| Animate Animation.Msg
--| UpdateQuery Hash
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
        GotZone (Ok zone) ->
            ( { model | utc = zone }, Cmd.none )

        GotZone (Err _) ->
            ( { model | utc = Time.utc }, Cmd.none )

        GotCommitHash (Ok hash) ->
            let
                newRepo =
                    { repo | head = Just hash }
            in
            ( { model
                | repo = newRepo
                , session = Session.updateRepo model.key newRepo model.session
              }
            , Task.attempt GotLog <| fetchHistory 10 url hash []
            )

        GotCommitHash (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка обновления истории хранилища" ] }, Cmd.none )

        GotDAG (Ok dag) ->
            ( { model | zipper = Success <| setFocus location dag }
            , case Dict.get currentPath.location model.repo.unsaved of
                Just changed ->
                    Content.fetchByCid url changed.cid GotNodeContent

                Nothing ->
                    Content.fetch url currentPath GotNodeContent
            )

        GotDAG (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки дерева классификатора" ] }, Cmd.none )

        GotLog (Ok log) ->
            ( { model | log = Success log }, Cmd.none )

        GotLog (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка загрузки истории изменений" ] }, Cmd.none )

        RemoveFocus zipper ->
            case Zipper.removeTree zipper of
                Just parent ->
                    let
                        removedNode =
                            Zipper.label zipper

                        node =
                            Zipper.label parent

                        newRepo =
                            { repo | location = node.location, unsaved = Dict.remove removedNode.location model.repo.unsaved }
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
            , checkNodeForChanges url repo.tree node
            )

        UpdateParent parent (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка при обновлении поля links у родителя (UpdateParent)" ] }, Cmd.none )

        Append zipper ->
            let
                label =
                    Zipper.label zipper

                child =
                    newChild zipper

                newLinks =
                    Zipper.children zipper
            in
            ( { model
                | zipper = Success (appendChild zipper) --updateRemote appendChild model.zipper
                , repo = { repo | unsaved = Dict.insert child.location child model.repo.unsaved }
                , session = Session.updateRepo model.key { repo | location = child.location } model.session
              }
            , Cmd.none
            )

        AddBookmark path string ->
            ( { model | notifications = [] }, Cmd.none )

        KeyDowns code (Success zipper) ->
            let
                node =
                    Zipper.label zipper
            in
            ( { model
                | zipper = Success <| keyAction code zipper
                , repo = { repo | unsaved = Dict.insert node.location node model.repo.unsaved }
              }
            , Cmd.none
            )

        KeyDowns _ _ ->
            ( model, Cmd.none )

        UpdateFocus node ->
            let
                replaceLabel z =
                    setFocus node.location z
                        |> Zipper.replaceLabel node
                        |> setFocus location
            in
            ( { model
                | zipper = updateRemote replaceLabel model.zipper
              }
            , checkNodeForChanges url repo.tree node
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
                [ Content.fetchByCid url node.cid GotNodeContent
                , Session.store <| Session.updateRepo model.key newRepo model.session
                ]
            )

        NodeChanged node (Err _) ->
            ( model, Cmd.none )

        GotUpdatedContentHash node (Ok cid) ->
            if cid == node.cid then
                let
                    newRepo =
                        { repo | unsaved = Dict.remove node.location model.repo.unsaved }
                in
                ( { model | repo = newRepo, session = Session.updateRepo model.key newRepo model.session }, Cmd.none )

            else
                update (UpdateFocus { node | cid = cid }) model

        {- let
               replaceLabel z =
                   setFocus node.location z
                       |> Zipper.replaceLabel { node | cid = cid }
                       |> setFocus location

               unsaved =
                   Dict.insert node.location { node | cid = cid } model.repo.unsaved
           in
           ( { model
               | zipper = updateRemote replaceLabel model.zipper
               , repo = { repo | unsaved = unsaved }
               , session = Session.updateRepo model.key { repo | unsaved = unsaved } model.session
             }
           , checkNodeForChanges url repo.tree { node | cid = cid }
           )
        -}
        GotUpdatedContentHash node (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса хэша файлов (GotUpdatedContentHash Msg)" ] }, Cmd.none )

        ChangeFocus node ->
            let
                ( haveNoChildren, newZipper ) =
                    case model.zipper of
                        Success zipper ->
                            ( List.isEmpty <| Zipper.children <| setFocus node.location zipper
                            , Success <| Zipper.replaceLabel node <| setFocus node.location zipper
                            )

                        _ ->
                            ( False, model.zipper )
            in
            if node.location == model.repo.location then
                ( { model | zipper = newZipper }
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
                        getChildren url model.repo.tree { node | expanded = True }
                            |> Task.attempt (AddTree node.location)

                      else
                        Cmd.none
                    , Api.post
                        (Endpoint.filesWrite url "/suzdal/repos.json")
                        (Api.jsonToHttpBody <| Repo.reposEncoder model.session.repos)
                        (\_ -> NoOp)
                    ]
                )

        ClearNotifications ->
            ( { model | notifications = [] }, Cmd.none )

        -- just update zipper without any other actions
        AddTree to (Ok tree) ->
            let
                replace z =
                    setFocus to z
                        |> Zipper.replaceTree tree
                        |> setFocus model.repo.location
            in
            ( { model | zipper = updateRemote replace model.zipper }, Cmd.none )

        AddTree to (Err _) ->
            ( { model | notifications = [ ServerError "Не удалось загрузить дерево. Попробуйте ещё раз" ] }, Cmd.none )

        RecursivePin node ->
            ( model
            , Api.get
                (Endpoint.pinAdd url node.links)
                (PinAddResponse node.links)
                (Decode.field "Pins" <| Decode.list Decode.string)
            )

        PinAddResponse hash (Ok _) ->
            ( { model | notifications = [ PinAdd ("Успешно сохранён " ++ hash) ] }, Cmd.none )

        PinAddResponse hash (Err _) ->
            ( { model | notifications = [ ServerError ("Не удалось сохранить " ++ hash) ] }, Cmd.none )

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

        --UpdateQuery hash ->
        --    ( { model | repo = Success ( Editing, hash ) }, Cmd.none )
        GotNewRootHash (Ok cid) ->
            let
                newPath =
                    { currentPath | cid = cid }
            in
            ( { model
                | repo = { repo | tree = cid }
                , session = Session.updateRepo model.key { repo | tree = cid } model.session
              }
            , Cmd.batch
                [ fetchDAG url newPath
                , Content.fetch url newPath GotNodeContent
                , Session.store <| Session.updateRepo model.key { repo | tree = cid } model.session
                , createCommit url model.session.settings.author repo "commit message"
                ]
            )

        GotNewRootHash (Err _) ->
            ( { model | notifications = [ ServerError "Ошибка запроса корневого хэша (GotNewRootHash Msg)" ] }
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
            ( { model
                | zipper = LoadingSlowly
                , repo = { repo | unsaved = Dict.empty }
              }
            , Cmd.batch
                [ Dict.values model.repo.unsaved
                    |> List.reverse
                    |> commitChanges (Endpoint.dagPut url) zipper
                    |> Task.andThen
                        (\patched_zipper ->
                            Decode.at [ "Cid", "/" ] Decode.string
                                |> Api.task "POST" (Endpoint.dagPut url) (rootbody patched_zipper)
                        )
                    |> Task.attempt GotNewRootHash
                , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
                ]
            )

        GotSession _ ->
            ( model, Cmd.none )

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

                        RemoveAll ->
                            List.filter (\a -> False)

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
       GotRepo (Ok r) ->
           case r.tree of
               Just hash ->
                   let
                       path =
                           { cid = hash, location = r.path }
                   in
                   ( { model | path = path, repo = r }, fetchDAG url path )

               Nothing ->
                   ( { model | repo = Just r }, Cmd.none )

       GotRepo (Err e) ->
           ( { model | notifications = [ ServerError <| Decode.errorToString e ] }, Cmd.none )

      RetrieveLocalStorageRepos ->
          ( model, Api.retrieveObject "repos" )


   UpdateContent content result ->
       case result of
           Ok list ->
               ( { model | content = Success <| List.indexedMap (\i a -> { a | id = i }) (list ++ content) }
               , let
                   body =
                       contentEncoder (list ++ content)
                 in
                 Api.put url body (GotUpdatedContentHash <| contentSize content)
               )

           Err _ ->
               ( { model | notifications = [ ServerError "Ошибка загрузки файлов (UpdateContent Msg)" ] }, Cmd.none )

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
            ]
            [ viewNotifications model.notifications
            , viewMetaData model.repo
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
    let
        updateButton hash =
            el
                [ Background.color <| lightGrey 1.0
                , Border.rounded 5
                ]
            <|
                button False Icons.arrowRight (GotNewRootHash <| Ok hash)
    in
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
        , el [ width fill, Font.italic, Font.size 12 ] <| text ("( " ++ String.fromInt (Dict.size repo.unsaved) ++ " изменений)")
        , el [ padding 5, centerX, centerY, width <| px 30 ] <| html Icons.hash
        , el [ Font.color <| darkGrey 1.0 ] <| text repo.tree
        ]


viewChanges : List Int -> Changes -> Element Msg
viewChanges location changes =
    if Dict.isEmpty changes then
        none

    else
        row [ spacing 15, Font.size 12 ]
            [ el [] <| text "Список изменённых ячеек"
            , wrappedRow [ spacing 7 ] <| List.map (\( k, v ) -> viewCrumbAsButton location v) <| Dict.toList changes
            ]


viewDAG : Model -> DAG -> Element Msg
viewDAG model dag =
    let
        cellStyle alpha node =
            [ width fill
            , height fill
            , Background.color <| simpleColorCodeConverter node.color alpha
            , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
            , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
            , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
            , inFront <|
                el
                    [ Background.color <| black 0.8
                    , width <| px 2
                    , height <| px 2
                    , padding 3
                    , alignTop
                    , alignRight
                    , moveDown 3.0
                    , moveLeft 3.0
                    , transparent <| isNotChanged node
                    , Border.width 0
                    , Border.rounded 10
                    , Border.color <| simpleColorCodeConverter node.color alpha
                    ]
                    none
            ]

        style node =
            if
                --node is member of breadcrumbs list
                List.member node (getCrumbs dag [])
                    -- node in focus
                    || (node.location == model.repo.location)
                    -- focus location occur in-order and consecutively anywhere within the node location
                    || isInfixOf model.repo.location node.location
            then
                cellStyle 1.0 node

            else
                cellStyle 0.3 node
                    ++ [ Font.color <| darkGrey 1.0
                       ]

        isNotChanged node =
            not <| Dict.member node.location model.repo.unsaved

        isCrumb node =
            el (style node) <| viewCell node

        focus =
            Zipper.label dag

        haveParent z =
            case Zipper.removeTree z of
                Just parent ->
                    True

                Nothing ->
                    False
    in
    row
        [ width fill
        , height fill
        , spacing 40
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
            ]
            [ getContexts dag []
                |> List.map
                    (row [ width fill, height fill, spacing 5 ] << List.map isCrumb)
                |> column [ width fill, spacing 5 ]
            , viewNodeProps model.repo.tree focus model.session.settings.shownodeprops
            ]
        , column
            [ alignTop, spacing 10 ]
            [ button (not <| haveParent dag) Icons.trash2 <| RemoveFocus dag
            , saveButton model.repo.unsaved dag
            , button False Icons.download <| RecursivePin (Zipper.label dag)
            , button False Icons.plusCircle <| Append dag
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
        [ button False Icons.fileText (AddText content)
        , button False Icons.filePlus (Pick node content)

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


viewCrumbs : List Int -> DAG -> Element Msg
viewCrumbs location zipper =
    getCrumbs zipper []
        |> List.map (viewCrumbAsButton location)
        |> row [ spacing 7 ]


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


viewCrumbAsButton : List Int -> Node -> Element Msg
viewCrumbAsButton location node =
    el
        [ width shrink
        , padding 5
        , Border.rounded 10
        , Border.width 2
        , if location == node.location then
            Border.color <| darkGrey 1.0

          else
            Border.color <| lightGrey 0.5
        , mouseOver [ Background.color <| lightGrey 0.5 ]
        , Background.color <| simpleColorCodeConverter node.color 0.7
        , Event.onClick <| ChangeFocus node
        , pointer
        ]
    <|
        text node.description


viewCell : Node -> Element Msg
viewCell node =
    if node.editing then
        Input.multiline
            [ height fill
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
            ]
        <|
            paragraph
                [ centerY
                , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , htmlAttribute <| Html.Attributes.style "white-space" "nowrap"
                , htmlAttribute <| Html.Attributes.style "text-overflow" "clip"
                ]
                [ text node.description ]


viewNodeProps : Hash -> Node -> Bool -> Element Msg
viewNodeProps root node show =
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
            ]

    else
        none


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


saveButton : Changes -> DAG -> Element Msg
saveButton changes dag =
    let
        noChanges =
            Dict.isEmpty changes
    in
    el
        [ Background.color <|
            if noChanges then
                lightGrey 1.0

            else
                orange 1.0
        , Border.rounded 5
        , Dict.size changes
            |> String.fromInt
            |> text
            |> el
                [ alignTop
                , alignRight
                , Font.size 10
                , Font.color <| white 1.0
                , Background.color <| black 0.8
                , Border.rounded 10
                , padding 3
                , moveRight 4
                , moveUp 4
                ]
            |> inFront
        ]
    <|
        button noChanges Icons.save (DagPut dag)


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
        |> Task.andThen (getChildren url path.cid)
        |> Task.andThen (\tree -> Task.succeed <| Zipper.fromTree <| expandFocus tree)
        |> Task.andThen (fetchZipper url <| pathlist path)
        |> Task.attempt GotDAG


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


checkNodeForChanges : Url -> Hash -> Node -> Cmd Msg
checkNodeForChanges url roothash node =
    Api.task "GET" (Endpoint.node url { cid = roothash, location = node.location }) Http.emptyBody nodeDecoder
        |> Task.andThen (Task.succeed << compareNodes node)
        |> Task.attempt (NodeChanged node)


compareNodes : Node -> Node -> Bool
compareNodes new old =
    (old.description /= new.description)
        || (old.links /= new.links)
        || (old.cid /= new.cid)
        || (old.color /= new.color)


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


updateRemote : (a -> a) -> Remote a -> Remote a
updateRemote fun z =
    case z of
        Success x ->
            Success (fun x)

        _ ->
            z


setFocus : List Int -> DAG -> DAG
setFocus location zipper =
    case Zipper.findFromRoot (\x -> x.location == location) zipper of
        Just z ->
            z

        Nothing ->
            zipper


getChildren : Url -> String -> Node -> Task Http.Error (Tree Node)
getChildren url root node =
    Decode.list nodeToTree
        |> Api.task "GET" (Endpoint.dagGet url node.links) Http.emptyBody
        --|> Api.task "GET" (Endpoint.links { cid = root, location = node.location }) Http.emptyBody
        |> Task.andThen (\list -> Task.succeed <| indexChildren <| Tree.tree node list)


getTree : Url -> String -> Int -> Tree Node -> Task Http.Error (Tree Node)
getTree url root depth tree =
    if depth == 0 then
        Task.succeed tree

    else
        Tree.label tree
            |> getChildren url root
            |> Task.andThen
                (\x ->
                    Tree.children x
                        |> List.map
                            (\child ->
                                getTree url root (depth - 1) child
                            )
                        |> Task.sequence
                )
            |> Task.andThen (\list -> Task.succeed <| Tree.replaceChildren list tree)


createCommit : Url -> Repo.Author -> Repo -> String -> Cmd Msg
createCommit url author repo comment =
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
        |> Task.attempt GotCommitHash



--createLogEntry : Path -> String -> String -> String -> Cmd Msg
--createLogEntry path field old new =
--    let
--        diff =
--            { path = path.location, field = field, old = old, new = new }
--    in
--    if new /= old then
--        Task.map2 (\time zone -> { time = time, zone = zone, diff = diff }) Time.now Time.here
--            |> Task.perform AddLogEntry
--    else
--        Cmd.none


fetchZipper : Url -> List Path -> DAG -> Task Http.Error DAG
fetchZipper url paths zipper =
    case paths of
        x :: xs ->
            let
                focus =
                    setFocus x.location zipper
            in
            Zipper.label focus
                |> getChildren url x.cid
                |> Task.andThen
                    (\tree ->
                        Zipper.replaceTree tree focus
                            |> Zipper.mapLabel (\label -> { label | expanded = not label.expanded })
                            |> fetchZipper url xs
                    )

        [] ->
            Zipper.root zipper
                |> Task.succeed


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


ipldLinkDecoder : Decode.Decoder Hash
ipldLinkDecoder =
    Decode.at [ "links", "/" ] Decode.string


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


hslSlider : Float -> Element Msg
hslSlider hue =
    Input.slider
        [ height (px 30)
        , behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color <| lightGrey 1.0
                , Border.rounded 2
                ]
                none
            )
        ]
        { onChange = \_ -> NoOp
        , label = Input.labelAbove [] <| text <| "Цветовой регулятор ( " ++ String.fromFloat hue ++ " )"
        , min = 0
        , max = 59.9999999
        , step = Nothing
        , value = hue
        , thumb =
            Input.defaultThumb
        }


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


index : List { a | id : Int } -> List { a | id : Int }
index =
    List.indexedMap (\i a -> { a | id = i })


expandFocus : Tree Node -> Tree Node
expandFocus =
    Tree.mapLabel (\label -> { label | expanded = not label.expanded })


pathlist : Path -> List Path
pathlist path =
    Page.processPath { path | location = List.reverse path.location } (\x -> x) []


navigateZipper : (DAG -> Maybe DAG) -> DAG -> DAG
navigateZipper fun zipper =
    case fun zipper of
        Just new ->
            new

        Nothing ->
            zipper


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
    , expanded = True
    }


appendChild : DAG -> DAG
appendChild zipper =
    let
        child =
            newChild zipper

        tree =
            Zipper.tree zipper
                |> Tree.appendChild (Tree.tree child [])
    in
    Zipper.replaceTree tree zipper


getCrumbs : DAG -> List Node -> List Node
getCrumbs zipper acc =
    let
        appendLabel =
            Zipper.label zipper :: acc
    in
    case Zipper.parent zipper of
        Just parent ->
            getCrumbs parent appendLabel

        Nothing ->
            appendLabel


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
            [ [ Zipper.label zipper ] ] ++ appendChildren


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
