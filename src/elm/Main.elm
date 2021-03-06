module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Html
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Repo as Repo
import Page.Settings as Settings
import Page.Welcome as Welcome
import Repo exposing (Repo)
import Result
import Route
import Session exposing (Session)
import UI.Layout as Layout
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound Session
    | Home Session
    | Settings Settings.Model
    | Repo String Repo.Model
    | Welcome Welcome.Model



-- MODEL


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init value url navKey =
    let
        decodeResult =
            Decode.decodeValue Decode.string value
                |> Result.andThen (Decode.decodeString (Session.sessionDecoder url))
    in
    case decodeResult of
        Ok session ->
            stepUrl url { key = navKey, page = Home session }

        Err _ ->
            ( Model navKey (Welcome { session = Session.default url, name = "", email = "" }), Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg doc =
            let
                { title, body } =
                    Page.view page doc
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.page of
        Welcome welcome ->
            { title = "Добро пожаловать в Суздаль"
            , body = List.map (Html.map GotWelcomeMsg) <| Layout.toHtml <| Welcome.view welcome
            }

        Home home ->
            { title = "Список репозиториев Суздаль"
            , body = List.map (Html.map GotHomeMsg) <| Layout.toHtml <| Home.view home
            }

        Settings settings ->
            viewPage Page.Settings GotSettingsMsg (Settings.view settings)

        Repo name repo ->
            viewPage (Page.Repo name) GotRepoMsg (Repo.view repo)

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view



-- UPDATE


type Msg
    = Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotRepoMsg Repo.Msg
    | GotWelcomeMsg Welcome.Msg


toSession : Model -> Session
toSession { page } =
    case page of
        NotFound session ->
            session

        Home session ->
            session

        Settings settings ->
            Settings.toSession settings

        Repo _ repo ->
            Repo.toSession repo

        Welcome welcome ->
            Welcome.toSession welcome


stepUrl : Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            toSession model
    in
    case Route.fromUrl url of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Settings ->
            Settings.init session
                |> updateWith Settings GotSettingsMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just (Route.Repo name) ->
            case Session.getRepo name session.repos of
                Just repo ->
                    Repo.init name repo session
                        |> updateWith (Repo name) GotRepoMsg model

                Nothing ->
                    ( model, Cmd.none )

        Just Route.Welcome ->
            Welcome.init session
                |> updateWith Welcome GotWelcomeMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ChangedUrl url ->
            stepUrl url model

        GotSettingsMsg subMsg ->
            case model.page of
                Settings settings ->
                    Settings.update subMsg settings |> updateWith Settings GotSettingsMsg model

                _ ->
                    ( model, Cmd.none )

        GotRepoMsg subMsg ->
            case model.page of
                Repo name repo ->
                    Repo.update subMsg repo |> updateWith (Repo name) GotRepoMsg model

                _ ->
                    ( model, Cmd.none )

        GotHomeMsg subMsg ->
            case model.page of
                Home home ->
                    Home.update subMsg home |> updateWith Home GotHomeMsg model

                _ ->
                    ( model, Cmd.none )

        GotWelcomeMsg subMsg ->
            case model.page of
                Welcome welcome ->
                    Welcome.update subMsg welcome |> updateWith Welcome GotWelcomeMsg model

                _ ->
                    ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound _ ->
            Sub.none

        Settings settings ->
            Sub.map GotSettingsMsg (Settings.subscriptions settings)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Repo _ repo ->
            Sub.map GotRepoMsg (Repo.subscriptions repo)

        _ ->
            Sub.none



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
