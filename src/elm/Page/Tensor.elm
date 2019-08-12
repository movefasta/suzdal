module Page.Tensor exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Animation
import Api exposing (Hash)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Bytes
import Bytes.Encode
import Colors exposing (..)
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
import File.Select
import Filesize
import Html
import Html.Attributes
import Http
import Icons
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra
import Loading
import MimeType as Mime
import Page
import Page.Settings as Settings
import Result exposing (Result)
import Route exposing (Path, Route)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
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


init : Session -> Path -> ( Model, Cmd Msg )
init session path =
    let
        expandFocus =
            Tree.mapLabel (\label -> { label | expanded = not label.expanded })

        url =
            Session.url session
    in
    ( { session = session
      , problems = []
      , root = Success ( Completed, path.cid )
      , zipper = Loading
      , path = path
      , hover = False
      , content = Loading
      , log = []
      , changes = Dict.empty
      , shownodeprops = False
      , color = 0
      , settings = Nothing

      --, style = Animation.style [ Animation.opacity 0.0 ]
      }
    , Cmd.batch
        [ Api.task "GET" (Endpoint.dagGet url path.cid) Http.emptyBody nodeDecoder
            |> Task.andThen (getChildren url path.cid)
            |> Task.andThen (\tree -> Task.succeed <| Zipper.fromTree <| expandFocus tree)
            |> Task.andThen (fetchZipper url <| pathlist path)
            |> Task.attempt GotDAG
        , Api.storePath path
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- MODEL


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidatedField
    = Email
    | Password


type alias Model =
    { session : Session
    , settings : Maybe Settings.Model
    , problems : List Problem
    , root : Remote ( Status, Hash )
    , zipper : Remote DAG
    , path : Path
    , hover : Bool
    , content : Remote Content
    , log : List Entry
    , changes : Dict (List Int) Node
    , shownodeprops : Bool
    , color : Float

    --, style : Animation.State
    }


type alias Node =
    { id : Id
    , status : Status
    , name : String
    , cid : Hash
    , links : Hash
    , size : Int
    , description : String
    , mimetype : Maybe Mime.MimeType
    , color : Int
    , location : List Int
    , expanded : Bool
    }


type alias UploadedFile =
    { id : Id
    , status : Status
    , name : String
    , cid : Hash
    , size : Int
    , description : String
    , mimetype : Maybe Mime.MimeType
    }


type Status
    = Editing
    | Uploading Float
    | Completed
    | UploadingFail
    | Selected
    | Waiting
    | Changed
    | GotProgress


type Remote a
    = NotAsked
    | Loading
    | LoadingSlowly
    | Success a
    | Failed


type Action
    = Remove Node
    | Set Node
    | Move Node Int
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



-- OBJECT API TYPES FOR DECODE IPFS RESPONSES


type alias Link =
    { name : String
    , hash : String
    , size : Int
    }


type alias Object =
    { links : List Node
    , data : String
    }


type alias ObjectStat =
    { hash : String
    , numLinks : Int
    , blockSize : Int
    , linksSize : Int
    , dataSize : Int
    , cumulativeSize : Int
    }


type alias Id =
    Int


type alias DAG =
    Zipper Node


type alias Content =
    List Node


type Msg
    = NoOp
    | UpdateQuery Hash
    | DagPut DAG
    | GetRootHash (Result Http.Error Hash)
    | GetContentHash Node (Result Http.Error Hash)
    | GetNodeContent String (Result Http.Error Content)
    | AddText Content
    | ChangeFocus Node
    | UpdateFocus Node
    | DownloadAsJson String
    | Pick Node Content
    | GotFiles Node Content File (List File)
      --    | UpdateContent Content (Result Http.Error Content)
    | Perform Node Action
    | DragEnter
    | DragLeave
    | GotSession Session.Session
    | AddTree (List Int) (Result Http.Error (Tree Node))
    | ClearProblemLog
    | AddBookmark Path String
    | AddLogEntry Entry
    | GotDAG (Result Http.Error DAG)
    | Append DAG
    | RemoveFocus DAG
    | InvertShowNodeProps
    | UpdateParent DAG (Result Http.Error Node)
    | KeyDowns String (Remote DAG)
    | PassedSlowLoadThreshold
    | ChangeColor Float



--| Animate Animation.Msg


type alias Styles =
    { open : List Animation.Property
    , closed : List Animation.Property
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentPath =
            model.path

        url =
            Session.url model.session
    in
    case msg of
        {- Animate animMsg ->
           ( { model
               | style = Animation.update animMsg model.style
             }
           , Cmd.none
           )
        -}
        GotDAG result ->
            case result of
                Ok dag ->
                    ( { model | zipper = Success <| setFocus model.path.location dag }
                    , Api.get (Endpoint.content url currentPath) (GetNodeContent "init content request") contentDecoder
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка загрузки классификатора" ] }, Cmd.none )

        ChangeColor hue ->
            ( { model | color = hue }, Cmd.none )

        KeyDowns code remotezipper ->
            ( case remotezipper of
                Success zipper ->
                    let
                        label =
                            Zipper.label zipper
                    in
                    { model
                        | zipper = Success <| keyAction code zipper
                        , changes = Dict.insert label.location label model.changes
                    }

                _ ->
                    model
            , Cmd.none
            )

        InvertShowNodeProps ->
            ( { model | shownodeprops = not model.shownodeprops }
            , Cmd.none
            )

        --ExpandAll ->
        --    ( model, getTree model.root 2 (Tree.tree { label | expanded = True } []) |> Task.attempt AddTree )
        RemoveFocus zipper ->
            case Zipper.removeTree zipper of
                Just parent ->
                    let
                        node =
                            Zipper.label parent
                    in
                    ( { model | zipper = Success parent, path = { currentPath | location = node.location } }
                    , updateParent (Endpoint.dagPut url) parent |> Task.attempt (UpdateParent parent)
                    )

                Nothing ->
                    ( { model | problems = [ ServerError "Нечего больше удалять! (RemoveFocus Msg)" ] }, Cmd.none )

        UpdateParent parent result ->
            case result of
                Ok node ->
                    ( { model
                        | zipper = Success (Zipper.replaceLabel node parent)
                        , changes = Dict.insert node.location node model.changes
                      }
                    , Api.get (Endpoint.dagGet url node.cid) (GetNodeContent node.cid) contentDecoder
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка при обновлении поля links у родителя (UpdateParent Msg)" ] }, Cmd.none )

        Append zipper ->
            let
                child =
                    Zipper.children zipper
                        |> List.length
                        |> List.singleton
                        |> List.append (.location <| Zipper.label zipper)
                        |> appendedTree
                        |> Tree.label
            in
            ( { model
                | zipper = updateRemote appendChild model.zipper
                , changes = Dict.insert child.location child model.changes
              }
            , Cmd.none
            )

        AddBookmark path string ->
            ( { model | problems = [] }, Cmd.none )

        ChangeFocus node ->
            let
                ( notHaveChildren, newZipper ) =
                    case model.zipper of
                        Success zipper ->
                            ( List.isEmpty <| Zipper.children <| setFocus node.location zipper
                            , Success <| Zipper.replaceLabel node <| setFocus node.location zipper
                            )

                        _ ->
                            ( False, model.zipper )

                newPath =
                    { currentPath | location = node.location }
            in
            if node.location == model.path.location then
                ( { model | zipper = newZipper }
                , if node.status == Editing then
                    Dom.focus (Route.locationToString "/" node.location)
                        |> Task.attempt (\_ -> NoOp)

                  else
                    Cmd.none
                )

            else
                ( { model
                    | zipper = newZipper
                    , session = Session.update model.session newPath url
                    , path = newPath

                    --, style = Animation.interrupt [ Animation.set [ Animation.opacity 0.0 ] ] model.style
                  }
                , Cmd.batch
                    [ Api.get (Endpoint.dagGet url node.cid) (GetNodeContent node.cid) contentDecoder
                    , Api.storePath newPath
                    , if notHaveChildren then
                        getChildren url newPath.cid { node | expanded = True }
                            |> Task.attempt (AddTree node.location)

                      else
                        Cmd.none
                    ]
                )

        ClearProblemLog ->
            ( { model | problems = [] }, Cmd.none )

        -- just update zipper without any other actions
        AddTree location result ->
            case result of
                Ok tree ->
                    let
                        replace z =
                            setFocus location z
                                |> Zipper.replaceTree tree
                                |> setFocus model.path.location
                    in
                    ( { model | zipper = updateRemote replace model.zipper }, Cmd.none )

                Err _ ->
                    ( { model | problems = [ ServerError "Не удалось загрузить дерево. Попробуйте ещё раз" ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        Pick node content ->
            ( model, File.Select.files [ "*" ] (GotFiles node content) )

        GotFiles node content file files ->
            let
                size =
                    List.map File.size (file :: files)
                        |> List.sum
                        |> (+) (contentSize content)
            in
            ( { model | hover = False }
            , List.map (uploadFile <| Endpoint.add <| Session.url model.session) (file :: files)
                |> Task.sequence
                |> Task.andThen
                    (\list ->
                        let
                            body =
                                List.indexedMap (\i a -> { a | id = i }) (list ++ content)
                                    |> Encode.list contentEncoder
                                    |> createBodyFrom
                        in
                        Api.task "POST" (Endpoint.dagPut url) body (Decode.at [ "Cid", "/" ] Decode.string)
                    )
                |> Task.attempt (GetContentHash { node | size = size })
            )

        {-
           UpdateContent content result ->
               case result of
                   Ok list ->
                       ( { model | content = Success <| List.indexedMap (\i a -> { a | id = i }) (list ++ content) }
                       , let
                           body =
                               Encode.list contentEncoder (list ++ content)
                         in
                         Api.put url body (GetContentHash <| contentSize content)
                       )

                   Err _ ->
                       ( { model | problems = [ ServerError "Ошибка загрузки файлов (UpdateContent Msg)" ] }, Cmd.none )
        -}
        GetContentHash node result ->
            case result of
                Ok cid ->
                    let
                        replaceLabel z =
                            setFocus node.location z
                                |> Zipper.replaceLabel { node | cid = cid }
                                |> setFocus model.path.location
                    in
                    ( { model
                        | zipper = updateRemote replaceLabel model.zipper
                        , changes = Dict.insert node.location node model.changes
                      }
                    , if node.location == model.path.location then
                        Api.get (Endpoint.dagGet url cid) (GetNodeContent "init content request") contentDecoder

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка запроса хэша файлов (GetContentHash Msg)" ] }, Cmd.none )

        UpdateQuery hash ->
            ( { model | root = Success ( Editing, hash ) }, Cmd.none )

        GetRootHash result ->
            case result of
                Ok cid ->
                    let
                        newPath =
                            { currentPath | cid = cid }
                    in
                    ( { model
                        | root = Success ( Completed, cid )
                        , session = Session.update model.session newPath url
                        , path = newPath
                      }
                    , Cmd.batch
                        [ Route.replaceUrl (Session.navKey model.session) (Route.Tensor newPath)
                        , Api.get (Endpoint.content url newPath) (GetNodeContent newPath.cid) contentDecoder
                        , Api.storePath newPath
                        , createLogEntry currentPath "классификатор" currentPath.cid newPath.cid
                        ]
                    )

                Err _ ->
                    ( { model | root = model.root, problems = [ ServerError "Ошибка запроса корневого хэша (GetRootHash Msg)" ] }
                    , Cmd.none
                    )

        GetNodeContent hash result ->
            case result of
                Ok nodes ->
                    ( { model
                        | content = Success <| List.indexedMap (\i a -> { a | id = i }) nodes

                        --, style = Animation.queue [ Animation.to [ Animation.opacity 1.0 ] ] model.style
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | problems = [ ServerError <| "Ошибка запроса файлов (GetNodeContent Msg) по адресу " ++ hash ] }, Cmd.none )

        AddLogEntry entry ->
            ( { model
                | log = model.log ++ [ entry ]

                --, changes = Dict.insert entry.path.location entry.diff model.changes
              }
            , Cmd.none
            )

        DownloadAsJson str ->
            ( model, File.Download.string "136_structure.json" "application/json" str )

        AddText content ->
            ( { model | content = Success <| List.indexedMap (\i a -> { a | id = i }) <| [ initText ] ++ content }
            , Task.attempt (\_ -> NoOp) <| Dom.focus "file-id-0"
            )

        UpdateFocus node ->
            ( { model
                | zipper = updateRemote (Zipper.replaceLabel node) model.zipper
                , changes = Dict.insert node.location node model.changes
              }
            , Cmd.none
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
            ( { model | content = loading model.content, root = loading model.root, zipper = loading model.zipper }, Cmd.none )

        DagPut zipper ->
            let
                rootbody =
                    createBodyFrom << ipldNodeEncoder << Zipper.label
            in
            ( { model
                | root = Loading
                , zipper = LoadingSlowly
                , changes = Dict.empty
              }
            , Cmd.batch
                [ Dict.values model.changes
                    |> List.reverse
                    |> commitChanges (Endpoint.dagPut url) zipper
                    |> Task.andThen
                        (\patched_zipper ->
                            Decode.at [ "Cid", "/" ] Decode.string
                                |> Api.task "POST" (Endpoint.dagPut url) (rootbody patched_zipper)
                        )
                    |> Task.attempt GetRootHash
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
                                body =
                                    links |> Encode.list contentEncoder |> createBodyFrom
                            in
                            addText (Endpoint.add url) link.id link.description
                                |> Task.andThen
                                    (\x ->
                                        Api.task "POST" (Endpoint.dagPut url) body (Decode.at [ "Cid", "/" ] Decode.string)
                                    )
                                |> Task.attempt (GetContentHash node)

                        _ ->
                            Cmd.none

                _ ->
                    Api.put url (Encode.list contentEncoder links) (GetContentHash node)
            )



--VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    --let
    --     animation =
    --       Animation.render model.style
    --           |> List.map htmlAttribute
    --in
    { title = "Suzdal v0.1 test"
    , content =
        case model.root of
            Success _ ->
                column
                    [ spacing 15
                    , width fill
                    , height fill
                    ]
                    [ viewProblems model.problems
                    , viewRemote (spinner "Загрузка корневого хэша репозитория") (viewControls model.changes model.zipper) model.root
                    , viewRemote (spinner "Загрузка дерева репозитория") (viewDAG model) model.zipper
                    , row
                        [ alignBottom, width fill, padding 7, Background.color <| black 0.8, Font.size 12 ]
                        [ viewLog model.log
                        , el [ width shrink, Font.color <| white 1.0, paddingXY 5 0 ] <| text "v0.1.0"
                        ]
                    ]

            LoadingSlowly ->
                spinner "Вычисляем корневой хэш репозитория..."

            _ ->
                none
    }


viewDAG : Model -> DAG -> Element Msg
viewDAG model dag =
    let
        cellStyle alpha node =
            [ width fill
            , height fill
            , Background.color <| simpleColorCodeConverter node.color alpha
            , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
            , Event.onClick <| ChangeFocus node
            ]

        style node =
            if
                List.member node (getCrumbs dag [])
                    || node.location
                    == model.path.location
                    || isInfixOf model.path.location node.location
            then
                cellStyle 1.0 node

            else
                cellStyle 0.3 node ++ [ Font.color <| darkGrey 1.0 ]

        isCrumb node =
            el (style node) <| viewCell model.path node

        focus =
            Zipper.label dag

        haveParent z =
            case Zipper.removeTree z of
                Just parent ->
                    True

                Nothing ->
                    False

        url =
            Session.url model.session
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
            [ width fill
            , spacing 15
            , alignTop
            , height fill
            , scrollbarY
            ]
            [ getContexts dag []
                |> List.map
                    (row [ width fill, height fill, spacing 5 ] << List.map isCrumb)
                |> column [ width fill, spacing 5 ]
            , row
                [ centerX, spacing 10 ]
                [ button False Icons.plusCircle <| Append dag
                , button
                    False
                    (if model.shownodeprops then
                        Icons.eyeOff

                     else
                        Icons.eye
                    )
                    InvertShowNodeProps
                , button (not <| haveParent dag) Icons.trash2 <| RemoveFocus dag
                ]

            --, hslSlider model.color
            , viewNodeProps model.path.cid model.color focus model.shownodeprops
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
            , viewRemote (spinner "Загрузка файлов") (viewContent dag url) model.content
            ]
        ]


viewContent : DAG -> Url -> Content -> Element Msg
viewContent dag url content =
    column
        [ scrollbarY, width fill, height fill, paddingEach { edges | right = 10 } ]
    <|
        List.map (viewNodeAsFile url <| Zipper.label dag) content


line : Element Msg
line =
    el [ height <| px 3, Background.color <| lightGrey 0.5, centerY, width fill ] <| text ""


viewContentEditor : Node -> Content -> Element Msg
viewContentEditor node content =
    row
        [ alignRight ]
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
        , centerY
        ]
        [ text node.description ]


viewLog : List Entry -> Element Msg
viewLog entries =
    List.reverse entries
        |> List.take 3
        |> List.map viewEntry
        |> column
            [ spacing 5
            , width fill
            ]


viewEntry : Entry -> Element Msg
viewEntry entry =
    row
        [ spacing 5
        , paddingXY 7 0
        ]
        [ el [ Font.color <| white 1.0 ] <| Timestamp.view entry.zone entry.time
        , el [ Font.color <| white 1.0 ] <| text "обновлён: "
        , el [ Font.color <| white 1.0 ] <| text entry.diff.field
        , el [ Font.color <| white 1.0 ] <| text "старое значение : "
        , el [ Font.color <| orange 1.0 ] <| text entry.diff.old
        , el [ Font.color <| white 1.0 ] <| text "новое значение : "
        , el [ Font.color <| green 1.0 ] <| text entry.diff.new
        ]



--viewCrumbs : DAG -> Element Msg
--viewCrumbs zipper =
--    getCrumbs zipper []
--        |> List.map viewCrumbAsButton
--        |> row [ spacing 7 ]
--viewCrumbAsLink : Hash -> Node -> Element Msg
--viewCrumbAsLink root node =
--    link
--        [ width shrink
--        , padding 5
--        , Border.rounded 10
--        , Border.width 2
--        , Border.color <| lightGrey 0.5
--        , mouseOver [ Background.color <| lightGrey 0.5 ]
--        ]
--        { url = Route.pathToUrl { cid = root, location = node.location }
--        , label = paragraph [] [ text node.description ]
--        }
--viewCrumbAsButton : Node -> Element Msg
--viewCrumbAsButton node =
--    el
--        [ width shrink
--        , padding 5
--        , Border.rounded 10
--        , Border.width 2
--        , Border.color <| lightGrey 0.5
--        , mouseOver [ Background.color <| lightGrey 0.5 ]
--        , Event.onClick <| ChangeFocus node
--        ]
--    <|
--        text node.description


viewCell : Path -> Node -> Element Msg
viewCell path node =
    case node.status of
        Editing ->
            Input.multiline
                [ height fill
                , width fill
                , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
                , Event.onLoseFocus <| UpdateFocus { node | status = Selected }
                , Font.center
                , centerX
                , centerY
                ]
                { onChange = \new -> UpdateFocus { node | description = new }
                , text = node.description
                , placeholder = Just <| Input.placeholder [] <| el [] none
                , label = Input.labelHidden "Data input"
                , spellcheck = True
                }

        _ ->
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
                , padding 6
                , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
                , Event.onClick <| ChangeFocus node
                , Event.onDoubleClick <| ChangeFocus { node | status = Editing }
                ]
            <|
                paragraph [ centerY ] [ text node.description ]


viewNodeProps : Hash -> Float -> Node -> Bool -> Element Msg
viewNodeProps root hue node show =
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
    column
        [ width fill
        , spacing 10
        , Background.color <| lightGrey 1.0
        , Border.rounded 5
        , padding 10
        , transparent show
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

        --, Input.radioRow
        --    [ width fill
        --    , spacing 5
        --    ]
        --    { onChange = \new -> UpdateFocus { node | color = new }
        --    , selected = Just node.color
        --    , label = Input.labelAbove [ padding 3 ] (el [ Font.size 10 ] <| text "Цвет")
        --    , options =
        --        let
        --            option i x =
        --                el
        --                    [ width <| px 30
        --                    , height <| px 25
        --                    , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
        --                    , Background.color <| colorCodeConverter i hue 1.0
        --                    , Border.color <|
        --                        case x of
        --                            Input.Idle ->
        --                                white 0
        --                            Input.Focused ->
        --                                lightGrey 0.8
        --                            Input.Selected ->
        --                                darkGrey 1.0
        --                    ]
        --                <|
        --                    text ""
        --        in
        --        List.range 0 9
        --            |> List.map
        --                (\code ->
        --                    Input.optionWith code (option code)
        --                )
        --    }
        , link
            [ Border.color <| black 1.0
            , Font.underline
            , padding 5
            , width fill
            ]
            { url = Route.pathToUrl { cid = root, location = node.location }
            , label = el [ Font.size 10 ] <| text "Абсолютная ссылка на ячейку"
            }
        ]


button : Bool -> Html.Html Msg -> Msg -> Element Msg
button disabled icon msg =
    Input.button
        [ padding 5
        , mouseOver <| [ Background.color <| lightGrey 1.0 ]
        , alignLeft
        , Border.rounded 5
        ]
        { onPress =
            if not disabled then
                Just msg

            else
                Nothing
        , label = html <| icon
        }


spinner : String -> Element Msg
spinner str =
    el [ centerX, centerY, width fill, height fill ] <| Loading.icon str


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewNodeActions : Url -> Node -> Node -> Element Msg
viewNodeActions url node file =
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
            [ if file.status /= Selected then
                htmlAttribute <| Html.Attributes.style "visibility" "hidden"

              else
                alignRight
            , spacing 5
            , padding 5
            ]
            [ newTabLink
                actionStyle
                { url = Endpoint.file url file.cid
                , label = text "открыть"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Set { file | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = file.name
                , url = Endpoint.file url file.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Move file -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Move file 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform node <| Remove file
                , label = text "x"
                }
            ]


viewNodeAsFile : Url -> Node -> Node -> Element Msg
viewNodeAsFile url node file =
    let
        style =
            case file.status of
                Selected ->
                    [ width fill
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| darkGrey 1.0
                    , Event.onDoubleClick <| Perform node <| Set { file | status = Editing }
                    , Event.onClick <| Perform node <| Set { file | status = Completed }

                    {- , Border.shadow
                       { offset = ( 2, 2 )
                       , size = 1
                       , blur = 3
                       , color = lightGrey 1.0
                       }
                    -}
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
                    , Event.onDoubleClick <| Perform node <| Set { file | status = Editing }
                    , Event.onClick <| Perform node <| Set { file | status = Selected }
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| white 1.0
                    ]
    in
    column
        [ width fill ]
        [ viewNodeActions url node file
        , el style <|
            case file.mimetype of
                Just (Mime.Video _) ->
                    column
                        [ width fill
                        , Font.color <| darkGrey 1.0
                        , mouseOver [ Font.color <| black 1.0 ]
                        ]
                        [ el [ width fill, padding 5, spacing 5, Font.italic ] <| text file.name
                        , html <|
                            Html.video
                                [ Html.Attributes.src <| Endpoint.file url file.cid
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
                        [ el [ width fill, padding 5, spacing 5, clip, Font.italic, Font.center ] <| text file.name
                        , html <|
                            Html.audio
                                [ Html.Attributes.src <| Endpoint.file url file.cid
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
                        { src = Endpoint.file url file.cid
                        , description = file.name
                        }

                Just (Mime.Text Mime.PlainText) ->
                    if file.status == Editing then
                        Input.multiline
                            ([ height (shrink |> minimum 30)
                             , width fill
                             , padding 5
                             , spacing 5
                             , Border.widthEach edges
                             , Border.color <| white 1.0
                             , Border.rounded 0
                             , htmlAttribute <| Html.Attributes.id <| String.concat [ "file-id-", String.fromInt file.id ]
                             , Event.onLoseFocus <|
                                if String.isEmpty file.description then
                                    Perform node <| Remove file

                                else
                                    Perform node <| Set { file | status = Changed }
                             ]
                                ++ fontBy file.size
                            )
                            { onChange =
                                \new -> Perform node <| Set { file | description = new, size = String.length new }
                            , text = file.description
                            , placeholder = Just <| Input.placeholder [] <| el [] none
                            , label = Input.labelHidden "Text data input"
                            , spellcheck = True
                            }

                    else
                        paragraph
                            ([ width fill, padding 5, spacing 5, clip, mouseOver [ Background.color <| lightGrey 0.2 ] ] ++ fontBy file.size)
                            [ text file.description ]

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
                            text file.name
                        , el
                            [ Font.color <| darkGrey 1.0
                            , padding 5
                            , alignRight
                            , centerY
                            ]
                          <|
                            text <|
                                "("
                                    ++ Filesize.format file.size
                                    ++ ")"
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


saveButton : Dict (List Int) Node -> DAG -> Element Msg
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


viewControls : Dict (List Int) Node -> Remote DAG -> ( Status, Hash ) -> Element Msg
viewControls changes zipper root =
    let
        updateButton hash =
            el
                [ Background.color <| lightGrey 1.0
                , Border.rounded 5
                ]
            <|
                button False Icons.arrowRight (GetRootHash <| Ok hash)
    in
    row
        [ spacing 5
        , padding 5
        , width fill
        , height shrink
        , Background.color <| lightGrey 0.2
        ]
    <|
        case root of
            ( Editing, string ) ->
                [ el [ padding 5, centerX, centerY ] <| html Icons.edit
                , el
                    [ height fill
                    , width fill
                    , centerY
                    , Font.color <| darkGrey 1.0
                    ]
                  <|
                    Input.text
                        [ width fill
                        , spacing 5
                        , padding 4
                        , Border.widthEach { edges | bottom = 2 }
                        , Border.color <| darkGrey 1.0
                        , Font.color <| darkGrey 1.0
                        , Border.rounded 0
                        , Background.color <| white 0.0
                        ]
                        { onChange = UpdateQuery
                        , text = string
                        , placeholder = Just <| Input.placeholder [] <| text "Введите идентификатор хранилища (хэш)"
                        , label = Input.labelAbove [] <| none
                        }
                , updateButton string
                ]

            ( Completed, string ) ->
                [ el [ padding 5, centerX, centerY ] <| html Icons.hash
                , el
                    [ padding 5
                    , height shrink
                    , width fill
                    , centerY
                    , Font.color <| darkGrey 1.0
                    , Event.onDoubleClick (UpdateQuery string)
                    ]
                  <|
                    text string
                , case zipper of
                    Success z ->
                        saveButton changes z

                    _ ->
                        none
                ]

            ( _, _ ) ->
                [ el [ width fill, Font.center ] <| text "Загружаем хранилище..." ]



--, Input.button
--    style
--    { onPress = Just <| DownloadAsJson <| Encode.encode 4 treeToJson
--    , label = text "скачать"
--    }
--, Input.button
--    style
--    { onPress = Just <| ExpandAll
--    , label = text "развернуть таблицу"
--    }


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
        [ viewCell path (Tree.label tree)
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
                [ viewCell path child_node ]
                    ++ (List.map
                            (\x ->
                                if node.expanded then
                                    viewCell path <| Tree.label x

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
            viewCell path node
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


viewProblems : List Problem -> Element Msg
viewProblems problems =
    let
        style =
            [ width fill
            , padding 5
            , Background.color <| orange 1.0
            ]

        render problem =
            case problem of
                InvalidEntry _ str ->
                    el style <| text str

                ServerError str ->
                    el style <| text str
    in
    case problems of
        [] ->
            none

        _ ->
            row style
                [ column [] <| List.map render problems
                , Input.button
                    [ alignRight
                    , height fill
                    , Border.width 1
                    , Border.rounded 3
                    , Border.color <| black 1.0
                    , Background.color <| lightGrey 1.0
                    ]
                    { onPress = Just ClearProblemLog
                    , label = el [ padding 3, Font.bold ] <| text "OK"
                    }
                ]



-- REQUESTS


createBodyFrom : Encode.Value -> Http.Body
createBodyFrom value =
    let
        turnToBytesPart message mime json =
            Encode.encode 0 json
                |> Bytes.Encode.string
                |> Bytes.Encode.encode
                |> Http.bytesPart message mime
    in
    Http.multipartBody
        [ turnToBytesPart "whatever" "application/json" value ]


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
    let
        linksbody z =
            Zipper.children z |> List.map Tree.label |> Encode.list ipldNodeEncoder |> createBodyFrom
    in
    case Zipper.parent zipper of
        Just parent ->
            Decode.at [ "Cid", "/" ] Decode.string
                |> Api.task "POST" endpoint (linksbody parent)
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


updateParent : Endpoint -> DAG -> Task Http.Error Node
updateParent endpoint zipper =
    let
        linksbody z =
            Zipper.children z |> List.map Tree.label |> Encode.list ipldNodeEncoder |> createBodyFrom
    in
    Decode.at [ "Cid", "/" ] Decode.string
        |> Api.task "POST" endpoint (linksbody zipper)
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


addText : Endpoint -> Id -> String -> Task Http.Error Node
addText endpoint id string =
    let
        bytes =
            string
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        body =
            Http.multipartBody [ Http.bytesPart "whatever" "text/plain" bytes ]
    in
    objLinkDecoder
        |> Decode.andThen linkToNodeDecoder
        |> Api.task "POST" endpoint body
        |> Task.andThen
            (\link ->
                Task.succeed
                    { link
                        | mimetype = Mime.parseMimeType "text/plain"
                        , size = Bytes.width bytes
                        , description =
                            if Bytes.width bytes < 5000 then
                                string

                            else
                                ""
                        , status = Completed
                        , id = id
                    }
            )


uploadFile : Endpoint -> File -> Task Http.Error Node
uploadFile endpoint file =
    let
        body =
            Http.multipartBody [ Http.filePart "file" file ]

        decoder =
            objLinkDecoder
                |> Decode.andThen linkToNodeDecoder
    in
    Api.task "POST" endpoint body decoder
        |> Task.andThen (\x -> Task.succeed { x | mimetype = File.mime file |> Mime.parseMimeType })


createLogEntry : Path -> String -> String -> String -> Cmd Msg
createLogEntry path field old new =
    let
        diff =
            { path = path.location, field = field, old = old, new = new }
    in
    Task.map2 (\time zone -> { time = time, zone = zone, diff = diff }) Time.now Time.here
        |> Task.perform AddLogEntry


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


pathlist : Path -> List Path
pathlist path =
    Page.processPath { path | location = List.reverse path.location } (\x -> x) []



-- DECODERS


treeDecoder : Decode.Decoder (Tree Node)
treeDecoder =
    Decode.map2 Tree.tree nodeDecoder lazyLinksDecoder
        |> Decode.map indexChildren


fullTreeDecoder : Decode.Decoder (Tree Node)
fullTreeDecoder =
    Decode.map2 Tree.tree fileDecoder lazyLinksDecoder


linksDecoder : Decode.Decoder (List (Tree Node))
linksDecoder =
    Decode.field "links" (Decode.list nodeToTree)


lazyLinksDecoder : Decode.Decoder (List (Tree Node))
lazyLinksDecoder =
    Decode.field "links" (Decode.lazy (\_ -> Decode.list treeDecoder))


nodeToTree : Decode.Decoder (Tree Node)
nodeToTree =
    Decode.map2 Tree.tree nodeDecoder (Decode.succeed [])



-- decode links field as ipld-link


ipldLinkDecoder : Decode.Decoder Hash
ipldLinkDecoder =
    Decode.at [ "links", "/" ] Decode.string


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> optional "name" Decode.string "0"
        |> required "cid" (Decode.field "/" Decode.string)
        |> required "links" (Decode.field "/" Decode.string)
        |> optional "size" sizeDecoder 0
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0
        -- node location in tree, ex. ["0","1","2"]
        |> hardcoded []
        |> hardcoded False


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.list fileDecoder


fileDecoder : Decode.Decoder Node
fileDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> optional "name" Decode.string "0"
        |> required "cid" (Decode.field "/" Decode.string)
        |> optional "links" (Decode.field "/" Decode.string) ""
        |> optional "size" sizeDecoder 0
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0
        -- node location in tree, ex. ["0","1","2"]
        |> hardcoded []
        |> hardcoded True


sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


linkToNodeDecoder : Link -> Decode.Decoder Node
linkToNodeDecoder link =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> hardcoded link.name
        |> hardcoded link.hash
        |> optional "links" Decode.string ""
        |> hardcoded link.size
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing
        |> optional "color" Decode.int 0
        |> hardcoded []
        |> hardcoded True


objLinkDecoder : Decode.Decoder Link
objLinkDecoder =
    Decode.succeed Link
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" sizeDecoder


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list nodeDecoder)
        |> required "Data" Decode.string


contentSize : Content -> Int
contentSize content =
    List.map .size content
        |> List.sum


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
        { onChange = ChangeColor
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



-- ENCODERS


ipldNodeEncoder : Node -> Encode.Value
ipldNodeEncoder node =
    Encode.object
        [ ( "cid", Encode.object [ ( "/", Encode.string node.cid ) ] )
        , ( "description", Encode.string node.description )
        , ( "links", Encode.object [ ( "/", Encode.string node.links ) ] )
        , ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "color", Encode.int node.color )
        ]


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


contentEncoder : Node -> Encode.Value
contentEncoder node =
    Encode.object
        [ ( "name", Encode.string node.name )
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
        , ( "mimetype", Encode.string <| Mime.toString <| Maybe.withDefault (Mime.OtherMimeType "") node.mimetype )
        , ( "description", Encode.string node.description )
        ]


pureContentEncoder : Node -> Encode.Value
pureContentEncoder node =
    Encode.object
        [ ( "description", Encode.string node.description )
        , ( "links", Encode.object [ ( "/", Encode.string node.cid ) ] )
        ]



-- HELPERS


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
        index location =
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
    Tree.mapChildren (index <| .location <| Tree.label tree) tree


appendChild : DAG -> DAG
appendChild zipper =
    let
        label =
            Zipper.label zipper

        child =
            Zipper.children zipper
                |> List.length
                |> List.singleton
                |> List.append label.location
                |> appendedTree

        tree =
            Zipper.tree zipper
                |> Tree.appendChild child
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
                    \_ -> Just <| Zipper.replaceLabel { label | status = Editing } zipper

                "Escape" ->
                    \_ -> Just <| Zipper.replaceLabel { label | status = Completed } zipper

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



-- CONSTANTS


initText : Node
initText =
    { id = 0
    , status = Editing
    , name = ""
    , cid = ""
    , links = ""
    , size = 0
    , description = ""
    , mimetype = Just (Mime.Text Mime.PlainText)
    , color = 0
    , location = []
    , expanded = True
    }


appendedTree : List Int -> Tree Node
appendedTree newLoc =
    Tree.tree
        { id = 0
        , status = Completed
        , name = "0"
        , cid = "zdpuAtQy7GSHNcZxdBfmtowdL1d2WAFjJBwb6WAEfFJ6T4Gbi" --empty content list
        , links = "zdpuAtQy7GSHNcZxdBfmtowdL1d2WAFjJBwb6WAEfFJ6T4Gbi" --empty links list
        , size = 0
        , description = ""
        , mimetype = Nothing
        , color = 0
        , location = newLoc
        , expanded = True
        }
        []


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
