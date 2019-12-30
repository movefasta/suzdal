module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Api.NodeConfig exposing (IpfsNodeConfig)
import Api.RepoStat exposing (RepoStat)
import Api.SwarmPeers exposing (SwarmPeers)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Email exposing (Email)
import Filesize
import Html.Attributes
import Html.Events as HtmlEvents
import Http
import Json.Decode as Decode exposing (Decoder, field, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra
import Log
import Process
import Repo exposing (Author)
import Session exposing (Session, Settings)
import Task
import Time
import UI.Button as Button
import UI.Colors as Colors exposing (..)
import UI.Icons as Icons
import Url exposing (Url)
import Username exposing (Username)


init : Session -> ( Model, Cmd Msg )
init session =
    let
        host =
            session.url
    in
    ( { session = session
      , problems = []
      , status = Loading
      , config = Loading
      , id = Loading
      , peers = []
      , swarm = Loading
      , repo = Loading
      , author = Loading
      }
    , Cmd.batch
        [ Api.get (Endpoint.config host) GotConfig Api.NodeConfig.decoder
        , Api.get (Endpoint.id host) GotNodeID idDecoder
        , Api.get (Endpoint.repoStat host) GotRepoStat Api.RepoStat.decoder
        , checkSwarmPeers host
        , Api.get (Endpoint.filesRead host "/suzdal/settings.json") GotAuthor Repo.authorDecoder
        , Process.sleep 0 |> Task.perform (always RetrieveLocalStoragePeers)
        ]
    )



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        decoder =
            Decode.decodeValue decodePeers

        retrieval ( _, json ) =
            GotPeers (decoder json)
    in
    Sub.batch [ Api.objectRetrieved retrieval ]



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , status : Status Form
    , config : Status IpfsNodeConfig
    , id : Status IpfsNodeID
    , peers : List Peer
    , swarm : Status SwarmPeers
    , repo : Status RepoStat
    , author : Status Author

    --, palette : Palette
    --, red : Float
    --, green : Float
    --, blue : Float
    --, alpha : Float
    }


type alias Palette =
    { yellow : Color
    , red : Color
    , violet : Color
    , blue : Color
    , cyan : Color
    , green : Color
    }


type alias Peer =
    { id : Int
    , hash : String
    , isRelay : Bool
    , connection : Connection
    , hover : Bool
    , editing : Bool
    , name : String
    }


type alias Form =
    { avatar : String
    , bio : String
    , email : String
    , username : String
    , password : String
    }


type Connection
    = Online SwarmStatus
    | Offline
    | Pending
    | NotAsked


type SwarmStatus
    = InSwarm
    | OutOfSwarm


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type alias IpfsNodeID =
    { id : String
    , publicKey : String
    , addresses : Maybe (List String)
    , agentVersion : String
    , protocolVersion : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidForm
    = Valid Form



-- UPDATE


type Msg
    = SubmittedForm Form
    | EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredBio String
    | EnteredAvatar String
    | CompletedFormLoad (Result Http.Error Form)
    | CompletedSave (Result Http.Error Hash)
    | GotSession Session
    | PassedSlowLoadThreshold
    | GotConfig (Result Http.Error IpfsNodeConfig)
    | GotNodeID (Result Http.Error IpfsNodeID)
    | PublishPeers (List Peer)
    | Published (Result Http.Error String)
    | GotPeerStatus Int (Result Http.Error String)
    | GotPeers (Result Decode.Error (List Peer))
    | GotAuthor (Result Http.Error Author)
    | Hover Peer Bool
    | AddPeer
    | UpdatePeer Peer
    | DeletePeer Int
    | SavePeer Peer
    | NoOp
    | Tick Time.Posix
    | CheckPeers (List Peer)
    | RetrieveLocalStoragePeers
    | GotSwarmPeers (Result Http.Error SwarmPeers)
    | GotRepoStat (Result Http.Error RepoStat)
    | InvertShowNodeProps Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        url =
            model.session.url

        settings =
            model.session.settings

        updateSettings s =
            ( { model | session = Session.updateSettings s model.session }
            , Session.store <| Session.updateSettings s model.session
            )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, Cmd.none )

        InvertShowNodeProps bool ->
            updateSettings { settings | shownodeprops = bool }

        --checkSwarmPeers url )
        RetrieveLocalStoragePeers ->
            ( model, Api.retrieveObject "peers" )

        Hover peer bool ->
            ( { model | peers = List.Extra.updateIf (\p -> p.id == peer.id) (\p -> { p | hover = bool }) model.peers }, Cmd.none )

        UpdatePeer updatedpeer ->
            ( { model | peers = List.Extra.updateIf (\p -> p.id == updatedpeer.id) (always updatedpeer) model.peers }, Cmd.none )

        SavePeer peertosave ->
            let
                newpeers =
                    List.Extra.updateIf (\p -> p.id == peertosave.id) (always peertosave) model.peers
            in
            ( { model | peers = newpeers }
            , Cmd.batch [ checkPeer "" url peertosave, Api.storePeers (encodePeers newpeers) ]
            )

        DeletePeer id ->
            let
                newpeers =
                    model.peers
                        |> List.filter (\peer -> peer.id /= id)
                        |> List.indexedMap (\i peer -> { peer | id = i })
            in
            ( { model | peers = newpeers }, Api.storePeers (encodePeers newpeers) )

        AddPeer ->
            let
                id =
                    List.length model.peers + 1

                newpeers =
                    anonymous id :: model.peers
            in
            ( { model | peers = newpeers }, Cmd.none )

        GotAuthor (Ok author) ->
            ( { model | author = Loaded author }, Cmd.none )

        GotAuthor (Err _) ->
            ( { model | problems = [ ServerError "GotAuthor Error" ] }, Cmd.none )

        GotPeerStatus id result ->
            let
                set connection peer =
                    if peer.id == id then
                        { peer | connection = connection }

                    else
                        peer
            in
            case result of
                Ok str ->
                    ( { model | peers = List.map (set (Online OutOfSwarm)) model.peers }, Cmd.none )

                Err problem ->
                    ( { model | peers = List.map (set Offline) model.peers }, Cmd.none )

        PublishPeers peers ->
            ( model, Cmd.none )

        --, Api.task "POST" (Endpoint.dagPut url) value Api.cidDecoder Api.get (Endpoint.publish url) Published <| Decode.field "Value" Decode.string
        Published result ->
            case result of
                Ok string ->
                    ( { model | problems = [ ServerError ("Список пиров опубликован, текущий адрес: " ++ string) ] }, Cmd.none )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка публикации списка пиров" ] }, Cmd.none )

        GotNodeID result ->
            case result of
                Ok data ->
                    ( { model | id = Loaded data }, Cmd.none )

                Err problems ->
                    ( { model | id = Failed }, Cmd.none )

        GotPeers result ->
            case result of
                Ok peers ->
                    ( { model | peers = peers }, Task.perform (\_ -> CheckPeers peers) Time.now )

                Err error ->
                    ( { model | problems = [ ServerError <| Decode.errorToString error ] }, Cmd.none )

        GotSwarmPeers result ->
            case result of
                Ok swarm ->
                    let
                        updatedpeers =
                            List.map (updateConnection swarm) model.peers
                    in
                    ( { model | swarm = Loaded swarm, peers = updatedpeers }, Cmd.batch <| List.map (checkPeer "" url) updatedpeers )

                Err _ ->
                    ( model, Cmd.none )

        GotRepoStat result ->
            case result of
                Ok stat ->
                    ( { model | repo = Loaded stat }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CheckPeers peers ->
            ( { model | peers = List.map (\p -> { p | connection = Pending }) peers }
            , Cmd.batch <| List.map (checkPeer "" url) peers
            )

        GotConfig result ->
            case result of
                Ok config ->
                    ( { model | config = Loaded config }, Cmd.none )

                Err problems ->
                    ( { model | config = Failed }, Cmd.none )

        CompletedFormLoad (Ok form) ->
            ( { model | status = Loaded form }
            , Cmd.none
            )

        CompletedFormLoad (Err _) ->
            ( { model | status = Failed }
            , Cmd.none
            )

        SubmittedForm form ->
            case validate form of
                Ok validForm ->
                    ( { model | status = Loaded form }
                    , Api.put url (body validForm) CompletedSave
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredBio bio ->
            updateForm (\form -> { form | bio = bio }) model

        EnteredAvatar avatar ->
            updateForm (\form -> { form | avatar = avatar }) model

        CompletedSave (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedSave (Ok hash) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Cmd.none
              -- Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            case model.status of
                Loading ->
                    ( { model | status = LoadingSlowly }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error )



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Настройки"
    , content =
        row
            [ width fill
            , height fill
            , Font.size 12
            , paddingXY 25 10
            , alignTop
            , spacing 20
            , Font.color <| Colors.black 1.0
            , htmlAttribute (Html.Attributes.style "flex-shrink" "1")
            , clip
            ]
            [ column
                [ width fill
                , height fill
                , alignTop
                , scrollbarY
                , spacing 40
                ]
                [ viewPeers model.swarm model.peers
                , viewSettings model.session.settings
                , viewRemote "Идентификационные данные" viewID model.id
                , viewRemote "Учётные данные пользователя" viewAuthor model.author
                , viewRemote "Статистика репозитория" viewRepoStat model.repo
                ]
            , viewRemote "Полная конфигурация" viewRawConfig model.config
            ]
    }


viewAuthor : Author -> Element Msg
viewAuthor author =
    column
        [ width fill, height fill, spacing 15, alignTop ]
        [ viewIdProperty "Имя пользователя" (text author.name)
        , viewIdProperty "Email" (text author.email)
        ]


viewSettings : Settings -> Element Msg
viewSettings settings =
    column
        [ width fill, height fill, spacing 15, alignTop ]
        [ header "Настройки отображения"
        , Input.checkbox []
            { onChange = InvertShowNodeProps
            , icon = Input.defaultCheckbox
            , checked = settings.shownodeprops
            , label = Input.labelRight [ spacing 7 ] <| text "Показывать подробную информацию о выбранной ячейке"
            }
        ]


viewRepoStat : RepoStat -> Element Msg
viewRepoStat stat =
    column
        [ alignTop, width fill, height shrink, spacing 10 ]
        [ viewIdProperty "Размер" (text <| Filesize.format <| Maybe.withDefault 0 stat.repoSize)
        , viewIdProperty "Максимальный размер" (text <| Filesize.format <| Maybe.withDefault 0 stat.storageMax)
        , viewIdProperty "Количество объектов" (text <| String.fromInt <| Maybe.withDefault 0 stat.numObjects)
        , viewIdProperty "Путь в файловой системе" (text <| Maybe.withDefault "" stat.repoPath)
        , viewIdProperty "Версия репозитория" (text <| Maybe.withDefault "" stat.version)
        ]


viewRemote : String -> (a -> Element Msg) -> Status a -> Element Msg
viewRemote title viewfun remote =
    column
        [ width fill, height fill, spacing 15, alignTop ]
        [ header title
        , case remote of
            Loaded a ->
                viewfun a

            Failed ->
                text "Ошибка загрузки"

            _ ->
                text "Загрузка ..."
        ]


viewPeers : Status SwarmPeers -> List Peer -> Element Msg
viewPeers response peers =
    column
        [ width fill, height fill, spacing 15, alignTop ]
        [ row
            [ spacing 10 ]
            [ header "Список избранных пиров"
            , Button.add AddPeer
            , Button.refresh (CheckPeers peers)
            , el [ Font.italic, paddingXY 7 0 ] <|
                text <|
                    case response of
                        Loaded swarm ->
                            "Всего онлайн в рое : " ++ String.fromInt (Api.SwarmPeers.amount swarm)

                        Failed ->
                            "Ошибка запроса списка пиров"

                        _ ->
                            "Запрос списка пиров..."
            ]
        , column [] <| List.map viewPeer peers
        ]


viewPeer : Peer -> Element Msg
viewPeer peer =
    let
        ( color, status ) =
            case peer.connection of
                Online swarmstatus ->
                    case swarmstatus of
                        InSwarm ->
                            ( Colors.green, "В сети, в рое" )

                        OutOfSwarm ->
                            ( Colors.green, "В сети, вне роя" )

                Offline ->
                    ( Colors.orange, "Недоступен" )

                Pending ->
                    ( Colors.darkGrey, "Соединяем..." )

                NotAsked ->
                    ( Colors.white, "" )

        shadow =
            Border.shadow
                { offset = ( 1, 1 )
                , size = 1
                , blur = 2
                , color = Colors.lightGrey 1.0
                }
    in
    column
        [ width fill
        , paddingXY 8 12
        , spacing 5
        , Event.onMouseEnter <| Hover peer True
        , Event.onMouseLeave <| Hover peer False
        , if peer.editing then
            shadow

          else
            mouseOver [ shadow ]
        ]
        [ row
            [ centerY
            , width fill
            , spacing 15
            ]
            [ el [ width <| px 30 ] <|
                html <|
                    if peer.isRelay then
                        Icons.server

                    else
                        Icons.user
            , column
                [ spacing 3
                , width fill
                ]
                [ row
                    [ spacing 5 ]
                    [ el [ Font.size 16 ] <| text peer.name, el [ centerY, Font.color <| color 1.0 ] <| text status ]
                , el [ Font.color <| Colors.darkGrey 1.0 ] <| text peer.hash
                ]
            , el
                [ transparent <| not (peer.hover || peer.editing)
                , Event.onClick <|
                    if peer.editing then
                        SavePeer { peer | editing = False }

                    else
                        UpdatePeer { peer | editing = True }
                , width <| px 30
                , pointer
                ]
              <|
                html <|
                    if peer.editing then
                        Icons.save

                    else
                        Icons.edit
            , el
                [ transparent <| not (peer.hover || peer.editing)
                , paddingXY 10 0
                , alignRight
                ]
              <|
                Button.delete False (DeletePeer peer.id)
            ]
        , if peer.editing then
            viewPeerProperties peer

          else
            none
        ]


viewPeerProperties : Peer -> Element Msg
viewPeerProperties peer =
    column
        [ width fill
        , spacing 4
        , padding 8
        ]
        [ Input.text
            [ width fill
            , spacing 5
            ]
            { onChange = \new -> UpdatePeer { peer | hash = new }
            , text = peer.hash
            , placeholder = Just <| Input.placeholder [] <| text "Идентификатор"
            , label = Input.labelAbove [] <| text "Идентификатор"
            }
        , Input.text
            [ width fill
            , spacing 5
            ]
            { onChange = \new -> UpdatePeer { peer | name = new }
            , text = peer.name
            , placeholder = Just <| Input.placeholder [] <| text "Ссылки"
            , label = Input.labelAbove [] <| text "Человекочитаемое имя"
            }
        , Input.checkbox
            []
            { onChange = \_ -> UpdatePeer { peer | isRelay = not peer.isRelay }
            , icon =
                \pred ->
                    el [ width <| px 30 ] <|
                        html <|
                            if pred then
                                Icons.checkSquare

                            else
                                Icons.square
            , checked = peer.isRelay
            , label =
                Input.labelRight
                    [ centerY ]
                <|
                    text "Узел является релейным"
            }
        ]


viewRawConfig : IpfsNodeConfig -> Element Msg
viewRawConfig config =
    el [ width fill, height fill, scrollbarY ] <|
        Input.multiline
            [ alignTop, width fill, height fill, scrollbarY ]
            { onChange = \_ -> NoOp
            , text = Api.NodeConfig.encoder config
            , placeholder = Nothing
            , label = Input.labelHidden "IPFS config show"
            , spellcheck = False
            }


viewIdProperty : String -> Element Msg -> Element Msg
viewIdProperty name value =
    row
        [ spacing 10 ]
        [ el [ Font.color <| Colors.darkGrey 1.0 ] <| text name
        , el [ padding 8, Background.color <| Colors.lightGrey 0.5, width fill ] value
        ]


viewSwarmConfig : Api.NodeConfig.Swarm -> Element Msg
viewSwarmConfig swarm =
    column
        [ alignTop, width fill, spacing 10 ]
        [ viewFlag "Disable Bandwidth Metrics" swarm.disableBandwidthMetrics
        , viewFlag "Disable NAT port map" swarm.disableNatPortMap
        , viewFlag "Disable Relay" swarm.disableRelay
        , viewFlag "Enable AutoNAT Service" swarm.enableAutoNATService
        , viewFlag "Enable Auto Relay" swarm.enableAutoRelay
        , viewFlag "Enable Relay Hop" swarm.enableRelayHop
        ]


viewFlag : String -> Maybe Bool -> Element Msg
viewFlag name value =
    case value of
        Just bool ->
            row
                [ width fill ]
                [ viewIdProperty name <|
                    text <|
                        if bool then
                            "true"

                        else
                            "false"
                ]

        Nothing ->
            none


viewID : IpfsNodeID -> Element Msg
viewID id =
    column
        [ alignTop, width fill, spacing 10 ]
        [ viewIdProperty "Идентификатор узла" (text id.id)
        , viewIdProperty "Адреса" <|
            case id.addresses of
                Just addresses ->
                    column [ spacing 3 ] <| List.map text addresses

                Nothing ->
                    text "У Вас нет адресов. Возможно, Вы работаете в оффлайн режиме"
        , viewIdProperty "Версия клиента" (text id.agentVersion)
        , viewIdProperty "Версия протокола" (text id.protocolVersion)
        ]


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



-- HELPERS


anonymous : Int -> Peer
anonymous id =
    { id = id
    , hash = ""
    , isRelay = False
    , connection = NotAsked
    , hover = False
    , name = "Анонимный будетлянин №" ++ String.fromInt id
    , editing = True
    }


maybeView : (a -> Element Msg) -> Maybe a -> Element Msg
maybeView viewer maybe =
    case maybe of
        Just value ->
            viewer value

        Nothing ->
            none


header : String -> Element Msg
header str =
    el
        [ Border.color <| Colors.darkGrey 1.0
        , Border.widthEach { edges | bottom = 2 }
        , Font.size 20
        , width shrink
        , paddingXY 0 10
        ]
    <|
        text str


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    HtmlEvents.on "keydown" (Decode.andThen isEnter HtmlEvents.keyCode)
        |> htmlAttribute



--colorSlider : Model -> Element Msg
--colorSlider model =
--    Input.slider
--        [ height (px 30)
--        , behindContent
--            (el
--                [ width fill
--                , height (px 2)
--                , centerY
--                , Background.color <| lightGrey 1.0
--                , Border.rounded 2
--                ]
--                none
--            )
--        ]
--        { onChange = ChangeColor
--        , label = Input.labelAbove [] <| text <| "Цветовой регулятор ( " ++ String.fromFloat hue ++ " )"
--        , min = 0
--        , max = 255
--        , step = 5
--        , value = model.activecolor
--        , thumb =
--            Input.defaultThumb
--        }
--colorOptions : Model -> Element Msg
--colorOptions model =
--        Input.radioRow
--            [ width fill
--            , spacing 5
--            ]
--            { onChange = \new -> UpdateFocus { model | activecolor = new }
--            , selected = Just model.color
--            , label = Input.labelAbove [ padding 3 ] (el [ Font.size 10 ] <| text "Цвет")
--            , options =
--                let
--                    option i x =
--                        el
--                            [ width <| px 30
--                            , height <| px 25
--                            , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
--                            , Background.color model.activecolor
--                            , Border.color <|
--                                case x of
--                                    Input.Idle ->
--                                        white 0
--                                    Input.Focused ->
--                                        lightGrey 0.8
--                                    Input.Selected ->
--                                        darkGrey 1.0
--                            ]
--                        <|
--                            text ""
--                in
--                List.range 0 9
--                    |> List.map
--                        (\code ->
--                            Input.optionWith code (option code)
--                        )
--            }
--decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
--decode decoder value =
--    -- It's stored in localStorage as a JSON String;
--    -- first decode the Value as a String, then
--    -- decode that String as JSON.
--    Decode.decodeValue Decode.string value
--        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)
-- Session.changes GotSession (Session.navKey model.session)
-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "username can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                let
                    passwordLength =
                        String.length form.password
                in
                if passwordLength > 0 then
                    [ "password must be at least " ++ " characters long." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { avatar = String.trim form.avatar
        , bio = String.trim form.bio
        , email = String.trim form.email
        , username = String.trim form.username
        , password = String.trim form.password
        }



-- IPFS node id encoder


encodePeers : List Peer -> Encode.Value
encodePeers list =
    let
        encoder peer =
            Encode.object
                [ ( "id", Encode.int peer.id )
                , ( "hash", Encode.string peer.hash )
                , ( "isRelay", Encode.bool peer.isRelay )
                , ( "name", Encode.string peer.name )
                ]
    in
    Encode.list encoder list


decodePeers : Decode.Decoder (List Peer)
decodePeers =
    Decode.list decodePeer


decodePeer : Decode.Decoder Peer
decodePeer =
    Decode.succeed Peer
        |> required "id" Decode.int
        |> required "hash" Decode.string
        |> required "isRelay" Decode.bool
        |> hardcoded NotAsked
        |> hardcoded False
        |> hardcoded False
        |> required "name" Decode.string



-- DECODERS


idDecoder : Decode.Decoder IpfsNodeID
idDecoder =
    Decode.succeed IpfsNodeID
        |> required "ID" Decode.string
        |> required "PublicKey" Decode.string
        |> optional "Addresses" (Decode.nullable (Decode.list Decode.string)) Nothing
        |> required "AgentVersion" Decode.string
        |> required "ProtocolVersion" Decode.string


formDecoder : Decoder Form
formDecoder =
    Decode.succeed Form
        |> required "image" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "bio" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "email" Decode.string
        |> required "username" Decode.string
        |> hardcoded ""


updateConnection : SwarmPeers -> Peer -> Peer
updateConnection swarm peer =
    if Api.SwarmPeers.inSwarm peer.hash swarm then
        { peer | connection = Online InSwarm }

    else
        { peer | connection = Pending }



-- HTTP


checkPeer : String -> Url -> Peer -> Cmd Msg
checkPeer relay url peer =
    case peer.connection of
        Pending ->
            Cmd.none

        _ ->
            Api.get (Endpoint.connect url peer.hash relay) (GotPeerStatus peer.id) (Decode.field "Strings" <| Decode.succeed "Ok")


checkSwarmPeers : Url -> Cmd Msg
checkSwarmPeers host =
    Api.get (Endpoint.swarmPeers host) GotSwarmPeers Api.SwarmPeers.decoder


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
body : TrimmedForm -> Encode.Value
body (Trimmed form) =
    let
        encodedAvatar =
            case form.avatar of
                "" ->
                    Encode.null

                avatar ->
                    Encode.string avatar

        updates =
            [ ( "username", Encode.string form.username )
            , ( "email", Encode.string form.email )
            , ( "bio", Encode.string form.bio )
            , ( "image", encodedAvatar )
            ]

        encodedUser =
            Encode.object <|
                case form.password of
                    "" ->
                        updates

                    password ->
                        ( "password", Encode.string password ) :: updates
    in
    Encode.object [ ( "user", encodedUser ) ]


nothingIfEmpty : String -> Maybe String
nothingIfEmpty str =
    if String.isEmpty str then
        Nothing

    else
        Just str
