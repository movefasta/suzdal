module Main exposing (main)

import Api exposing (Hash)
import Avatar exposing (Avatar)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Loading
import Page exposing (Page)
import Page.Blank as Blank
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Repo as Repo
import Page.Settings as Settings
import Page.Welcome as Welcome
import Repo exposing (Repo)
import Result exposing (Result)
import Route exposing (Route)
import Session exposing (Session)
import Time
import UI.Layout as Layout
import Url exposing (Url)
import Username exposing (Username)


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



--init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
--init url navKey =
--    ( { key = navKey, page = Home { session | settings = settings, url = url } }
--    , Api.get (Endpoint.filesRead host "/suzdal/settings.json") GotAuthor Repo.authorDecoder )
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
            { title = "Добро пожаловать в Суз-Даль"
            , body = List.map (Html.map GotWelcomeMsg) <| Layout.toHtml <| Welcome.view welcome
            }

        Home home ->
            { title = "Список репозиториев"
            , body = List.map (Html.map GotHomeMsg) <| Layout.toHtml <| Home.view home
            }

        Settings settings ->
            viewPage Page.Settings GotSettingsMsg (Settings.view settings)

        Repo name repo ->
            viewPage (Page.Repo name) GotRepoMsg (Repo.view repo)

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view



--Loading ->
--    { title = "Список репозиториев"
--    , body = Layout.toHtml <| Loading.spinner "Инициализация"
--    }
-- UPDATE


type Msg
    = Ignored
      --    | GotAuthor (Result Http.Error Repo.Author)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg
    | GotRepoMsg Repo.Msg
    | GotWelcomeMsg Welcome.Msg
    | GotRepos (Result Http.Error (List Repo))


toSession : Model -> Session
toSession { key, page } =
    case page of
        NotFound session ->
            session

        Home session ->
            session

        Settings settings ->
            Settings.toSession settings

        Repo name repo ->
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
        {- GotAuthor (Ok author) ->
               ( model
               , Cmd.none
                 --Api.get (Endpoint.filesRead host "/suzdal/settings.json") GotRepos Repo.authorDecoder
               )

           GotAuthor (Err _) ->
               ( Model navKey (Welcome { session = session, name = "", email = "" }), Cmd.none )
        -}
        GotRepos (Ok repos) ->
            ( model, Cmd.batch [ Route.replaceUrl model.key Route.Home ] )

        GotRepos (Err e) ->
            ( model, Cmd.none )

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
