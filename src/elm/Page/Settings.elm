module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Api.NodeConfig exposing (IpfsNodeConfig)
import Avatar
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Colors exposing (..)
import Dict exposing (Dict)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Email exposing (Email)
import Html
import Html.Attributes
import Html.Events as HtmlEvents
import Http
import Icons
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Loading
import Log
import Route
import Session exposing (Session, url)
import Task
import Url exposing (Url)
import Username as Username exposing (Username)



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , status : Status Form
    , config : Status IpfsNodeConfig
    , id : Status IpfsNodeID
    , shownodeprops : Bool
    , peers : List Peer
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
    = Online
    | Offline
    | Pending


type alias Profile =
    { id : Hash
    , name : String
    , avatar : Hash
    , desciption : String
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type alias IpfsNodeID =
    { id : String
    , publicKey : String
    , addresses : List String
    , agentVersion : String
    , protocolVersion : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , problems = []
      , status = Loading
      , config = Loading
      , id = Loading
      , shownodeprops = False
      , peers = []
      }
    , Cmd.batch
        [ Api.get (Endpoint.config <| Session.url session) GotConfig Api.NodeConfig.decoder
        , Api.get (Endpoint.id <| Session.url session) GotNodeID idDecoder
        , Api.fetchPeers ()
        ]
    )


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Настройки"
    , content =
        row
            [ width shrink
            , Font.size 12
            , paddingXY 25 10
            , alignTop
            , spacing 20
            , Font.color <| black 1.0
            , Font.family
                [ Font.typeface "Ubuntu-Regular"
                , Font.sansSerif
                ]
            ]
            [ column
                [ width shrink, spacing 15, alignTop ]
                [ row
                    [ spacing 10 ]
                    [ header "Список пиров"
                    , el
                        [ Event.onClick AddPeer, pointer, width <| px 22, height <| px 22 ]
                      <|
                        html Icons.plusCircle
                    ]
                , viewPeers model.peers
                ]
            , column
                [ width shrink, spacing 15, alignTop ]
                [ header "Идентификационные данные", viewID model.id ]
            ]
    }


header : String -> Element Msg
header str =
    el
        [ Border.color <| darkGrey 1.0
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


viewPeers : List Peer -> Element Msg
viewPeers peers =
    column [] <| List.map viewPeer peers


viewPeer : Peer -> Element Msg
viewPeer peer =
    let
        ( color, status ) =
            case peer.connection of
                Online ->
                    ( Colors.green, "В сети" )

                Offline ->
                    ( Colors.orange, "Недоступен" )

                Pending ->
                    ( Colors.darkGrey, "Соединяем..." )
    in
    column
        [ width fill
        , paddingXY 8 12
        , spacing 5
        , mouseOver
            [ Border.shadow
                { offset = ( 1, 1 )
                , size = 1
                , blur = 2
                , color = lightGrey 1.0
                }
            ]
        ]
        [ row
            [ centerY
            , width fill
            , spacing 15
            , Event.onMouseEnter <| Hover peer.id True
            , Event.onMouseLeave <| Hover peer.id False
            ]
            [ el [] <|
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
                [ transparent <| not peer.hover
                , Event.onClick <| UpdatePeer { peer | editing = not peer.editing }
                , paddingXY 10 0
                , pointer
                ]
              <|
                html Icons.edit
            , el
                [ transparent <| not peer.hover
                , Event.onClick <| DeletePeer peer.id
                , paddingXY 10 0
                , alignRight
                , pointer
                ]
              <|
                html Icons.delete
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


viewConfig : Status IpfsNodeConfig -> Element Msg
viewConfig fetched =
    case fetched of
        Loading ->
            text "Загрузка конфигурационного файла..."

        LoadingSlowly ->
            text "Загрузка медленная ..."

        Loaded config ->
            Api.NodeConfig.encoder config
                |> Html.text
                |> html
                |> el [ alignTop, width fill, height shrink, Font.size 14 ]

        Failed ->
            text "Ошибка запроса конфигурационного файла узла"


viewIdProperty : String -> Element Msg -> Element Msg
viewIdProperty name value =
    column
        [ spacing 10 ]
        [ el [ Font.color <| Colors.darkGrey 1.0, Font.size 11 ] <| text name
        , el [ padding 8, Background.color <| Colors.lightGrey 0.5, width fill ] value
        ]


viewID : Status IpfsNodeID -> Element Msg
viewID status =
    case status of
        Loading ->
            text "Загрузка идентификационной информации узла..."

        LoadingSlowly ->
            text "Загрузка медленная ..."

        Loaded id ->
            column
                [ alignTop, width fill, height shrink, spacing 10 ]
                [ viewIdProperty "Идентификатор узла" (text id.id)
                , viewIdProperty "Адреса" <| column [ Font.size 10, spacing 3 ] <| List.map text id.addresses
                , viewIdProperty "Версия клиента" (text id.agentVersion)
                , viewIdProperty "Версия протокола" (text id.protocolVersion)
                ]

        Failed ->
            text "Ошибка запроса идентификационной информации узла"


checkPeer : Int -> String -> String -> Url -> Cmd Msg
checkPeer id hash relay url =
    Api.get (Endpoint.connect url hash relay) (GotPeerStatus id) (Decode.field "Strings" <| Decode.succeed "Ok")



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
    | UpdateLocalStorage String
    | GotConfig (Result Http.Error IpfsNodeConfig)
    | GotNodeID (Result Http.Error IpfsNodeID)
    | PublishPeers (List Peer)
    | Published (Result Http.Error String)
    | GotPeerStatus Int (Result Http.Error String)
    | GotPeers (Result Decode.Error (List Peer))
    | Hover Int Bool
    | AddPeer
    | UpdatePeer Peer
    | DeletePeer Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        url =
            Session.url model.session
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Hover id bool ->
            let
                hover peer =
                    if peer.id == id then
                        { peer | hover = bool }

                    else
                        peer

                newpeers =
                    List.map hover model.peers
            in
            ( { model | peers = newpeers }, Cmd.none )

        UpdatePeer updatedpeer ->
            let
                toggle peer =
                    if peer.id == updatedpeer.id then
                        updatedpeer

                    else
                        peer

                newpeers =
                    List.map toggle model.peers
            in
            ( { model | peers = newpeers }
            , Cmd.batch
                [ checkPeer updatedpeer.id updatedpeer.hash "" url
                , Api.storePeers (encodePeers newpeers)
                ]
            )

        DeletePeer id ->
            let
                newpeers =
                    List.filter (\peer -> peer.id /= id) model.peers
            in
            ( { model | peers = newpeers }, Api.storePeers (encodePeers newpeers) )

        AddPeer ->
            let
                id =
                    List.length model.peers + 1

                newpeers =
                    { id = id
                    , hash = ""
                    , isRelay = False
                    , connection = Pending
                    , hover = False
                    , name = "Анонимный будетлянин"
                    , editing = True
                    }
                        :: model.peers
            in
            ( { model | peers = newpeers }, Cmd.none )

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
                    ( { model | peers = List.map (set Online) model.peers }, Cmd.none )

                Err problem ->
                    ( { model | peers = List.map (set Offline) model.peers }, Cmd.none )

        PublishPeers peers ->
            ( model
            , Cmd.none
              --, Api.task "POST" (Endpoint.dagPut url) value Api.cidDecoder Api.get (Endpoint.publish url) Published <| Decode.field "Value" Decode.string
            )

        Published result ->
            case result of
                Ok string ->
                    ( { model | problems = [ ServerError ("Список пиров опубликован. Текущий адрес: " ++ string) ] }, Cmd.none )

                Err _ ->
                    ( { model | problems = [ ServerError "Ошибка сохранения списка пиров" ] }, Cmd.none )

        GotNodeID result ->
            case result of
                Ok data ->
                    ( { model | id = Loaded data }
                    , Cmd.none
                      --Api.task "GET" (Endpoint.resolve url data.id) Http.emptyBody (Decode.field "Path" Decode.string)
                      --    |> Task.andThen (\str -> Api.task "GET" (Endpoint.dagGet url str) Http.emptyBody decodePeers)
                      --    |> Task.attempt GotPeers
                    )

                Err problems ->
                    ( { model | id = Failed }, Cmd.none )

        GotPeers result ->
            case result of
                Ok peers ->
                    ( { model | peers = peers }
                    , Cmd.batch <| List.map (\peer -> checkPeer peer.id peer.hash "" url) peers
                    )

                Err _ ->
                    ( { model | peers = model.peers }, Cmd.none )

        GotConfig result ->
            case result of
                Ok config ->
                    ( { model | config = Loaded config }, Cmd.none )

                Err problems ->
                    ( { model | config = Failed }, Cmd.none )

        UpdateLocalStorage hash ->
            ( model, Api.storeSettings { cid = hash, location = [] } )

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
                    , Api.put (Session.url model.session) (body validForm) CompletedSave
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
            ( model
            , Api.storeSettings { cid = hash, location = [] }
            )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        decoder value =
            Decode.decodeValue Decode.string value
                |> Result.andThen (Decode.decodeString decodePeers)
    in
    Api.getPeers (GotPeers << decoder)



--decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
--decode decoder value =
--    -- It's stored in localStorage as a JSON String;
--    -- first decode the Value as a String, then
--    -- decode that String as JSON.
--    Decode.decodeValue Decode.string value
--        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)
-- Session.changes GotSession (Session.navKey model.session)
-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



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


encodeIpfsNodeID : IpfsNodeID -> Encode.Value
encodeIpfsNodeID x =
    Encode.object
        [ ( "ID", Encode.string x.id )
        , ( "PublicKey", Encode.string x.publicKey )
        , ( "Addresses", makeListEncoder Encode.string x.addresses )
        , ( "AgentVersion", Encode.string x.agentVersion )
        , ( "ProtocolVersion", Encode.string x.protocolVersion )
        ]


decodePeers : Decode.Decoder (List Peer)
decodePeers =
    Decode.list decodePeer


decodePeer : Decode.Decoder Peer
decodePeer =
    Decode.succeed Peer
        |> required "id" Decode.int
        |> required "hash" Decode.string
        |> required "isRelay" Decode.bool
        |> hardcoded Pending
        |> hardcoded False
        |> hardcoded False
        |> required "name" Decode.string



--- encoder helpers


makeListEncoder : (a -> Encode.Value) -> List a -> Encode.Value
makeListEncoder f arr =
    Encode.list f arr


makeDictEncoder : (a -> Encode.Value) -> Dict String a -> Encode.Value
makeDictEncoder f dict =
    Encode.object (Dict.toList (Dict.map (\k -> f) dict))


makeNullableEncoder : (a -> Encode.Value) -> Maybe a -> Encode.Value
makeNullableEncoder f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Encode.null



-- DECODERS


idDecoder : Decode.Decoder IpfsNodeID
idDecoder =
    Decode.succeed IpfsNodeID
        |> required "ID" Decode.string
        |> required "PublicKey" Decode.string
        |> required "Addresses" (Decode.list Decode.string)
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



-- HTTP


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
