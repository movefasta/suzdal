module Page.Welcome exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Api.Endpoint as Endpoint
import Element exposing (..)
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Http
import Loading
import Repo exposing (Author)
import Route
import Session exposing (Session)
import UI.Button as Button
import UI.Colors exposing (..)
import Url exposing (Url)



-- MODEL


type alias Model =
    { session : Session
    , name : String
    , email : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    --( Model session session.settings.author.name session.settings.author.email, Cmd.none )
    ( Model session "" "", Cmd.none )



-- COMMANDS


saveAuthor : Url -> Author -> Cmd Msg
saveAuthor url author =
    Api.post (Endpoint.filesWrite url "/suzdal/settings.json") (Api.jsonToHttpBody <| Repo.authorEncoder author) (\_ -> NoOp)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = NoOp
    | EnteredName String
    | EnteredEmail String
    | Save
    | SavedToStorage (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model

        url =
            session.url

        author =
            { name = model.name, email = model.email }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnteredName name ->
            ( { model | name = name }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        Save ->
            ( { model | session = Session.updateAuthor author model.session }
            , Cmd.batch
                [ saveAuthor url author
                , Session.store (Session.updateAuthor author session)
                ]
            )

        SavedToStorage _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    column
        [ centerX
        , centerY
        , spacing 40
        , width <| px 500
        ]
        [ el [ centerX ] (Loading.logo 100)
        , paragraph [ spacing 10, Font.size 16 ] <|
            List.map text <|
                [ "О, здравствуй, новый будетлянин! "
                , "Представься, пожалуйста. "
                , "Так остальным пользователям трактовочной сети будет проще понять кто ты такой"
                ]
        , column
            [ spacing 20
            , padding 2
            , width fill
            ]
            [ Input.text
                [ Event.onLoseFocus Save ]
                { onChange = EnteredName
                , text = model.name
                , placeholder = Just <| Input.placeholder [] <| text "Имя"
                , label = Input.labelAbove [] <| none
                }
            , Input.text
                [ Event.onLoseFocus Save ]
                { onChange = EnteredEmail
                , text = model.email
                , placeholder = Just <| Input.placeholder [] <| text "Email"
                , label = Input.labelAbove [] <| none
                }
            ]
        , link
            [ Border.color <| black 1.0
            , alignRight

            --, transparent <| String.isEmpty <| model.name ++ model.email
            ]
            { url = Route.routeToString Route.Home
            , label = Button.next Save
            }
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--let
--    retrieval ( _, json ) =
--        GotRepo (Decode.decodeValue repoDecoder json)
--in
--Sub.batch
--    [ Events.onKeyPress (Decode.map (\x -> KeyDowns x model.zipper) keyDecoder)
--    , Api.objectRetrieved retrieval
--    --, Animation.subscription Animate [ model.style ]
--    ]
