module Page.Tensor exposing (Model, Msg, init, subscriptions, toSession, update, view)

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
        [ Events.onKeyPress (Decode.map KeyDowns keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



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
      , zipper = Zipper.fromTree <| initTree []
      , upload = []
      , path = path
      , hover = False
      , content = Loading
      , log = []
      , changes = Dict.empty
      , shownodeprops = False
      }
    , Cmd.batch
        [ Api.task "GET" (Endpoint.dagGet url path.cid) Http.emptyBody nodeDecoder
            |> Task.andThen (getChildren url path.cid)
            |> Task.andThen (\tree -> Task.succeed <| Zipper.fromTree <| expandFocus tree)
            |> Task.andThen (fetchZipper url <| pathlist path)
            |> Task.andThen (\z -> Task.succeed <| Zipper.tree z)
            |> Task.attempt AddTree
        , Api.storeSettings path
        , Api.get (Endpoint.content url path) (GetNodeContent "init content request") contentDecoder
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )


fetchZipper : Url -> List Path -> Zipper Node -> Task Http.Error (Zipper Node)
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



-- CONSTANTS


initNode : List Int -> Node
initNode location =
    { id = 0
    , status = Completed
    , name = "ROOT"
    , cid = ""
    , links = ""
    , size = 0
    , description = "Кругозор"
    , mimetype = Nothing
    , color = 0
    , location = location
    , expanded = True
    }


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


initTree : List Int -> Tree Node
initTree location =
    Tree.tree
        (initNode location)
        []


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



-- MODEL


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidatedField
    = Email
    | Password


type alias Model =
    { session : Session
    , problems : List Problem
    , root : Remote ( Status, Hash )
    , zipper : Zipper Node
    , upload : List File
    , path : Path
    , hover : Bool
    , content : Remote (List Node)
    , log : List Entry
    , changes : Dict (List Int) Node
    , shownodeprops : Bool
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


type alias IpfsNodeID =
    { id : String
    , publicKey : String
    , addresses : List String
    , agentVersion : String
    , protocolVersion : String
    }


type Status
    = Editing
    | Uploading Float
    | Completed
    | UploadingFail
    | Selected
    | Waiting
    | Changed


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


type Msg
    = NoOp
    | UpdateQuery Hash
    | DagPut Encode.Value
    | GetRootHash (Result Http.Error Hash)
    | GetContentHash Int (Result Http.Error Hash)
    | GetNodeContent String (Result Http.Error (List Node))
    | AddText (List Node)
    | ChangeFocus Node
    | UpdateFocus Node
    | DownloadAsJson String
    | Pick
    | GotFiles File (List File)
    | UpdateContent (List Node) (Result Http.Error (List Node))
    | Perform Action
    | DragEnter
    | DragLeave
    | GotSession Session.Session
    | AddTree (Result Http.Error (Tree Node))
    | ClearProblemLog
    | AddBookmark Path String
    | AddLogEntry Entry
    | Append
    | RemoveFocus
    | InvertShowNodeProps
    | UpdateParent (Result Http.Error Node)
    | KeyDowns String
    | PassedSlowLoadThreshold



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        label =
            Zipper.label model.zipper

        currentPath =
            model.path

        url =
            Session.url model.session
    in
    case msg of
        KeyDowns code ->
            ( { model
                | zipper = keyAction code model.zipper
                , changes = Dict.insert label.location label model.changes
              }
            , Cmd.none
            )

        InvertShowNodeProps ->
            ( { model | shownodeprops = not model.shownodeprops }, Cmd.none )

        --ExpandAll ->
        --    ( model, getTree model.root 2 (Tree.tree { label | expanded = True } []) |> Task.attempt AddTree )
        RemoveFocus ->
            case removeNode model.zipper of
                Just parent ->
                    let
                        node =
                            Zipper.label parent
                    in
                    ( { model | zipper = parent, path = { currentPath | location = node.location } }
                    , updateParent (Endpoint.dagPut url) parent |> Task.attempt UpdateParent
                    )

                Nothing ->
                    ( { model | problems = [ ServerError "Нечего больше удалять! (RemoveFocus Msg)" ] }, Cmd.none )

        UpdateParent result ->
            case result of
                Ok node ->
                    ( { model
                        | zipper = setFocus node.location model.zipper |> Zipper.replaceLabel node
                        , changes = Dict.insert node.location node model.changes
                      }
                    , Api.get (Endpoint.dagGet url node.cid) (GetNodeContent node.cid) contentDecoder
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка при обновлении поля links у родителя (UpdateParent Msg)" ] }, Cmd.none )

        Append ->
            let
                child =
                    Zipper.children model.zipper
                        |> List.length
                        |> List.singleton
                        |> List.append label.location
                        |> appendedTree
                        |> Tree.label
            in
            ( { model
                | zipper = appendChild model.zipper
                , changes = Dict.insert child.location child model.changes
              }
            , Cmd.none
            )

        AddBookmark path string ->
            ( { model | problems = [] }, Cmd.none )

        ChangeFocus node ->
            let
                newZipper =
                    setFocus node.location model.zipper
                        |> Zipper.replaceLabel { node | expanded = True }

                newPath =
                    { currentPath | location = node.location }
            in
            if label.location == node.location then
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
                  }
                , Cmd.batch
                    [ Api.get (Endpoint.dagGet url node.cid) (GetNodeContent node.cid) contentDecoder
                    , Api.storeSettings newPath
                    , if List.isEmpty <| Zipper.children newZipper then
                        getChildren url newPath.cid { node | expanded = True }
                            |> Task.attempt AddTree

                      else
                        Cmd.none
                    ]
                )

        ClearProblemLog ->
            ( { model | problems = [] }, Cmd.none )

        -- just update zipper without any other actions
        AddTree result ->
            case result of
                Ok tree ->
                    ( { model
                        | zipper =
                            Zipper.replaceTree tree model.zipper
                                |> setFocus model.path.location
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Не удалось загрузить дерево. Попробуйте ещё раз" ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        Pick ->
            ( model, File.Select.files [ "*" ] GotFiles )

        GotFiles file files ->
            let
                decoder =
                    objLinkDecoder
                        |> Decode.andThen linkToNodeDecoder

                upload body =
                    Api.task "POST" (Endpoint.add <| Session.url model.session) (Http.multipartBody [ Http.filePart "file" body ]) decoder
                        |> Task.andThen
                            (\x ->
                                Task.succeed
                                    { x | mimetype = File.mime file |> Mime.parseMimeType }
                            )
            in
            ( { model | upload = [], hover = False }
            , case model.content of
                Success content ->
                    List.map upload (file :: files)
                        |> Task.sequence
                        |> Task.attempt (UpdateContent content)

                _ ->
                    Cmd.none
            )

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
                        , Api.get (Endpoint.content url newPath) (GetNodeContent model.path.cid) contentDecoder
                        , Api.storeSettings newPath
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
                    ( { model | content = Success <| List.indexedMap (\i a -> { a | id = i }) nodes }, Cmd.none )

                Err _ ->
                    ( { model | problems = [ ServerError <| "Ошибка запроса файлов (GetNodeContent Msg) по адресу " ++ hash ] }, Cmd.none )

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
                    ( { model | problems = [ ServerError "Ошибка загрузки файлов (Content Msg)" ] }, Cmd.none )

        GetContentHash size result ->
            case result of
                Ok cid ->
                    let
                        newLabel =
                            { label | cid = cid, size = size }

                        updatedZipper =
                            Zipper.replaceLabel newLabel model.zipper
                    in
                    ( { model
                        | zipper = updatedZipper
                        , changes = Dict.insert label.location newLabel model.changes
                      }
                    , Cmd.none
                      -- for debug purposes - verify added content by getting from IPFS by new hash
                      --let
                      --    body =
                      --        updatedZipper
                      --            |> Zipper.toTree
                      --            |> treeEncoder
                      --  in
                      --  Cmd.batch
                      --    [ Api.get (Endpoint.dagGet url cid) (GetNodeContent "request") contentDecoder ]
                    )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка запроса хэша файлов (GetContentHash Msg)" ] }, Cmd.none )

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
                | zipper = Zipper.replaceLabel node model.zipper
                , changes = Dict.insert node.location node model.changes
              }
            , Cmd.none
            )

        PassedSlowLoadThreshold ->
            let
                content =
                    case model.content of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                root =
                    case model.root of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | content = content, root = root }, Cmd.none )

        DagPut value ->
            let
                rootbody z =
                    Zipper.label z |> ipldNodeEncoder |> createBodyFrom
            in
            ( { model
                | root = Loading
                , zipper = Zipper.fromTree <| Tree.tree (initNode []) []
                , changes = Dict.empty
              }
            , Cmd.batch
                [ Dict.values model.changes
                    |> List.reverse
                    |> commitChanges (Endpoint.dagPut url) model.zipper
                    |> Task.andThen
                        (\zipper ->
                            Decode.at [ "Cid", "/" ] Decode.string
                                |> Api.task "POST" (Endpoint.dagPut url) (rootbody zipper)
                        )
                    |> Task.attempt GetRootHash
                , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
                ]
            )

        GotSession _ ->
            ( model, Cmd.none )

        Perform action ->
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

                        --List.map
                        --    (\x ->
                        --        if x.id == link.id then
                        --            link
                        --        else
                        --            x
                        --    )
                        RemoveAll ->
                            List.filter (\a -> False)

                ( links, size ) =
                    case model.content of
                        Success content ->
                            ( mapFun content, contentSize content )

                        _ ->
                            ( [], label.size )
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
                                |> Task.attempt (GetContentHash size)

                        _ ->
                            Cmd.none

                _ ->
                    Api.put url
                        (Encode.list contentEncoder links)
                        (GetContentHash size)
            )



--VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    let
        label =
            Zipper.label model.zipper

        depth =
            List.length model.path.location
    in
    { title = "Suz-Dal App v0.1"
    , content =
        column
            [ Font.size 14
            , Font.family
                [ Font.typeface "Ubuntu-Regular"
                , Font.sansSerif
                ]
            , spacing 15
            , width fill
            , height fill
            ]
            [ viewProblems model.problems
            , case model.root of
                Success ( _, _ ) ->
                    row
                        [ width fill
                        , spacing 20
                        , paddingEach { edges | top = 10, bottom = 10, left = 20 }
                        , height <| px 950
                        ]
                        [ viewAllContexts model
                        , viewContent model
                        ]

                LoadingSlowly ->
                    el [ width fill, height <| px 950, Background.color <| white 1.0 ] <|
                        el [ centerX, centerY ] <|
                            html Icons.loader

                _ ->
                    el [ centerX, centerY, width fill, height <| px 950, Background.color <| white 1.0 ] <|
                        none
            , viewLog model.log
            ]
    }


viewAllContexts : Model -> Element Msg
viewAllContexts model =
    let
        isCrumb a =
            if List.member a (getCrumbs model.zipper []) || (a.location == model.path.location) then
                [ Background.color <| colorCodeConverter a.color 1.0
                , Border.color <| darkGrey 1.0
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 1
                    , color = lightGrey 1.0
                    }
                ]

            else
                [ Background.color <| colorCodeConverter a.color 0.3, Border.color <| white 1.0, Border.width 2 ]
    in
    el
        [ width (fill |> minimum 800)
        , alignTop
        , height fill
        ]
    <|
        column
            [ width fill, spacing 15 ]
            [ viewControls model
            , getContexts model.zipper []
                |> List.map
                    (\list ->
                        List.map (\node -> el ([ width fill, height fill, centerY ] ++ isCrumb node) <| viewCell model.path node) list
                            |> row [ width fill, height fill, spacing 5 ]
                    )
                |> column [ width fill, spacing 5 ]
            , row
                [ centerX, spacing 10 ]
                [ button False Icons.plusCircle Append
                , button False Icons.trash2 RemoveFocus
                ]
            ]


viewLog : List Entry -> Element Msg
viewLog entries =
    List.reverse entries
        |> List.take 3
        |> List.map viewEntry
        |> column
            [ spacing 5
            , Font.size 12
            , width fill
            , height fill
            , padding 7
            , Background.color <| black 0.8
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



--viewCrumbs : Zipper Node -> Element Msg
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
                ]
                { onChange = \new -> UpdateFocus { node | description = new }
                , text = node.description
                , placeholder = Just <| Input.placeholder [] <| el [] none
                , label = Input.labelHidden "Data input"
                , spellcheck = True
                }

        _ ->
            paragraph
                [ mouseOver <|
                    [ Border.shadow
                        { offset = ( 0, 0 )
                        , size = 2
                        , blur = 4
                        , color = lightGrey 1.0
                        }
                    ]

                --, htmlAttribute <| Html.Attributes.property "transition" (Encode.string "box-shadow 333ms ease-in-out 0s, width 250ms, height 250ms, background-color 250ms")
                , pointer
                , Font.center
                , height (fill |> minimum 40)
                , width fill
                , padding 10
                , htmlAttribute <| Html.Attributes.id <| Route.locationToString "/" node.location
                , Event.onClick <| ChangeFocus node
                , Event.onDoubleClick <| ChangeFocus { node | status = Editing }
                ]
                [ text node.description ]


viewNode : Hash -> Node -> Element Msg
viewNode root node =
    column
        [ width fill
        , spacing 4
        , Background.color <| lightGrey 1.0
        , Border.rounded 5
        , padding 10
        ]
        [ link
            [ Border.color <| black 1.0
            , Font.underline
            , padding 5
            , width fill
            ]
            { url = Route.pathToUrl { cid = root, location = node.location }
            , label = el [ Font.size 10 ] <| text "Абсолютная ссылка на ячейку"
            }
        , Input.text
            [ width fill
            , spacing 5
            ]
            { onChange = \new -> UpdateFocus { node | cid = new }
            , text = node.cid
            , placeholder = Just <| Input.placeholder [] <| text "Идентификатор контента - хэш"
            , label = Input.labelAbove [] <| el [ Font.size 10 ] <| text "Адрес файлов"
            }
        , Input.text
            [ width fill
            , spacing 5
            ]
            { onChange = \new -> UpdateFocus { node | links = new }
            , text = node.links
            , placeholder = Just <| Input.placeholder [] <| text "Ссылки"
            , label = Input.labelAbove [] <| el [ Font.size 10 ] <| text "Адрес дочерних ячеек"
            }
        , Input.radioRow
            [ width fill
            , spacing 5
            ]
            { onChange = \new -> UpdateFocus { node | color = new }
            , selected = Just node.color
            , label = Input.labelAbove [ padding 3 ] (el [ Font.size 10 ] <| text "Цвет")
            , options =
                let
                    option i x =
                        el
                            [ width <| px 30
                            , height <| px 25
                            , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
                            , Background.color <| colorCodeConverter i 1.0
                            , Border.color <|
                                case x of
                                    Input.Idle ->
                                        white 0

                                    Input.Focused ->
                                        lightGrey 0.8

                                    Input.Selected ->
                                        darkGrey 1.0
                            ]
                        <|
                            text ""
                in
                List.range 0 9
                    |> List.map
                        (\code ->
                            Input.optionWith code (option code)
                        )
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


viewContent : Model -> Element Msg
viewContent model =
    let
        node =
            Zipper.label model.zipper

        tooltip str =
            [ above <| el [ Background.color <| black 1.0, Font.color <| white 1.0 ] <| text str ]
    in
    column
        [ width
            (fill
                |> maximum 800
                |> minimum 400
            )
        , alignTop
        ]
        [ column
            [ width fill
            , spacing 5
            , paddingEach { edges | right = 28, left = 15 }
            , height fill
            ]
            [ case model.content of
                Success content ->
                    column
                        [ height <| px 880
                        , width fill
                        ]
                        [ row
                            [ width fill ]
                            [ el [ alignLeft ] <| button False Icons.fileText (AddText content)
                            , el [ alignLeft ] <| button False Icons.filePlus Pick
                            , el [ alignLeft ] <|
                                button
                                    False
                                    (if model.shownodeprops then
                                        Icons.eyeOff

                                     else
                                        Icons.eye
                                    )
                                    InvertShowNodeProps
                            , el
                                [ alignRight, transparent (List.isEmpty content) ]
                              <|
                                button False Icons.trash2 <|
                                    Perform RemoveAll
                            ]
                        , if model.shownodeprops then
                            viewNode model.path.cid node

                          else
                            none
                        , paragraph
                            [ paddingEach { edges | top = 20, bottom = 7 }
                            , Border.color <| darkGrey 1.0
                            , Border.widthEach { edges | bottom = 2 }
                            , Font.size 24
                            ]
                            [ text node.description ]
                        , column
                            [ width fill, scrollbarY, alignTop, paddingEach { edges | right = 10 } ]
                          <|
                            List.map (viewNodeAsFile <| Session.url model.session) content
                        ]

                LoadingSlowly ->
                    el [ centerX, centerY, height fill ] <|
                        html Icons.loader

                _ ->
                    none
            ]
        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewNodeActions : Url -> Node -> Element Msg
viewNodeActions url node =
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
            [ if node.status /= Selected then
                htmlAttribute <| Html.Attributes.style "visibility" "hidden"

              else
                alignRight
            , spacing 5
            , padding 5
            ]
            [ newTabLink
                actionStyle
                { url = Endpoint.file url node.cid
                , label = text "открыть"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Set { node | status = Editing }
                , label = text "править"
                }
            , downloadAs
                actionStyle
                { label = text "загрузить"
                , filename = node.name
                , url = Endpoint.file url node.cid
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move node -1
                , label = text "˄"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Move node 1
                , label = text "˅"
                }
            , Input.button actionStyle
                { onPress = Just <| Perform <| Remove node
                , label = text "x"
                }
            ]


viewNodeAsFile : Url -> Node -> Element Msg
viewNodeAsFile url node =
    let
        style =
            case node.status of
                Selected ->
                    [ width fill
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| darkGrey 1.0
                    , Event.onDoubleClick <| Perform <| Set { node | status = Editing }
                    , Event.onClick <| Perform <| Set { node | status = Completed }
                    ]

                Editing ->
                    [ width fill
                    , paddingXY 10 0
                    ]

                _ ->
                    [ width fill
                    , Event.onDoubleClick <| Perform <| Set { node | status = Editing }
                    , Event.onClick <| Perform <| Set { node | status = Selected }
                    , Border.width 1
                    , Border.dashed
                    , Border.color <| white 1.0
                    , mouseOver [ Background.color <| lightGrey 0.2 ]
                    ]
    in
    column
        [ width fill ]
        [ viewNodeActions url node
        , el style <|
            case node.mimetype of
                Just (Mime.Video _) ->
                    el [ width fill ] <|
                        html <|
                            Html.video
                                [ Html.Attributes.src <| Endpoint.file url node.cid
                                , Html.Attributes.controls True
                                , Html.Attributes.style "width" "100%"
                                ]
                                []

                Just (Mime.Image _) ->
                    image [ width fill ]
                        { src = Endpoint.file url node.cid
                        , description = node.name
                        }

                Just (Mime.Text Mime.PlainText) ->
                    if node.status == Editing then
                        Input.multiline
                            ([ height fill
                             , width fill
                             , htmlAttribute <| Html.Attributes.id <| String.concat [ "file-id-", String.fromInt node.id ]
                             , Event.onLoseFocus <|
                                if String.isEmpty node.description then
                                    Perform <| Remove node

                                else
                                    Perform <| Set { node | status = Changed }
                             ]
                                ++ fontBy node.size
                            )
                            { onChange =
                                \new -> Perform <| Set { node | description = new, size = String.length new }
                            , text = node.description
                            , placeholder = Just <| Input.placeholder [] <| el [] none
                            , label = Input.labelHidden "Text data input"
                            , spellcheck = True
                            }

                    else
                        paragraph
                            ([ width fill, padding 5, spacing 5, clip ] ++ fontBy node.size)
                            [ text node.description ]

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
                            text node.name
                        , el
                            [ Font.color <| darkGrey 1.0
                            , padding 5
                            , alignRight
                            , centerY
                            ]
                          <|
                            text <|
                                "("
                                    ++ Filesize.format node.size
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


viewControls : Model -> Element Msg
viewControls model =
    let
        treeToJson =
            treeEncoder <| Zipper.toTree <| model.zipper

        noChanges =
            Dict.isEmpty model.changes

        updateButton hash =
            el
                [ Background.color <| lightGrey 1.0
                , Border.rounded 5
                ]
            <|
                button False Icons.arrowRight (GetRootHash <| Ok hash)

        saveButton =
            el
                [ Background.color <|
                    if noChanges then
                        lightGrey 1.0

                    else
                        orange 1.0
                , Border.rounded 5
                , Dict.size model.changes
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
                button noChanges Icons.save (DagPut treeToJson)
    in
    row
        [ spacing 5
        , width fill
        , height shrink
        , Font.size 18
        ]
    <|
        case model.root of
            Success ( Editing, string ) ->
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
                        ]
                        { onChange = UpdateQuery
                        , text = string
                        , placeholder = Just <| Input.placeholder [] <| text "Идентификатор контента - хэш"
                        , label = Input.labelAbove [] <| none
                        }
                , updateButton string
                ]

            Success ( Completed, string ) ->
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
                , saveButton
                ]

            _ ->
                []



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
        , scrollbarY
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


commitChanges : Endpoint -> Zipper Node -> List Node -> Task Http.Error (Zipper Node)
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


commitChange : Endpoint -> Zipper Node -> Task Http.Error (Zipper Node)
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


updateParent : Endpoint -> Zipper Node -> Task Http.Error Node
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


setFocus : List Int -> Zipper Node -> Zipper Node
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


createLogEntry : Path -> String -> String -> String -> Cmd Msg
createLogEntry path field old new =
    let
        diff =
            { path = path.location, field = field, old = old, new = new }
    in
    Task.map2 (\time zone -> { time = time, zone = zone, diff = diff }) Time.now Time.here
        |> Task.perform AddLogEntry



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


contentDecoder : Decode.Decoder (List Node)
contentDecoder =
    Decode.list fileDecoder


fileDecoder : Decode.Decoder Node
fileDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded Completed
        |> required "name" Decode.string
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


ipfsNodeID : Decode.Decoder IpfsNodeID
ipfsNodeID =
    Decode.succeed IpfsNodeID
        |> required "ID" Decode.string
        |> required "PublicKey" Decode.string
        |> required "Addresses" (Decode.list Decode.string)
        |> required "AgentVersion" Decode.string
        |> required "ProtocolVersion" Decode.string


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list nodeDecoder)
        |> required "Data" Decode.string


contentSize : List Node -> Int
contentSize content =
    List.map .size content
        |> List.sum


colorCodeConverter : Int -> Float -> Color
colorCodeConverter i alpha =
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


navigateZipper : (Zipper Node -> Maybe (Zipper Node)) -> Zipper Node -> Zipper Node
navigateZipper fun zipper =
    case fun zipper of
        Just new ->
            new

        Nothing ->
            zipper


addPathsToTree : Zipper Node -> Zipper Node
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


appendChild : Zipper Node -> Zipper Node
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


removeNode : Zipper Node -> Maybe (Zipper Node)
removeNode zipper =
    Zipper.removeTree zipper


getCrumbs : Zipper Node -> List Node -> List Node
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


getContexts : Zipper Node -> List (List Node) -> List (List Node)
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


keyAction : String -> Zipper Node -> Zipper Node
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
