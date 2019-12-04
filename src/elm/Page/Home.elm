module Page.Home exposing (Msg, init, subscriptions, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Dict exposing (Dict)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Email exposing (Email)
import File
import File.Select
import Html
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Loading
import Log
import PaginatedList exposing (PaginatedList)
import Process
import Repo exposing (Repo, repoDecoder, reposEncoder)
import Route exposing (Path)
import Session exposing (Session)
import Task exposing (Task)
import Time
import UI.Button as Button
import UI.Colors as Colors exposing (..)
import UI.Fonts
import UI.Icons as Icons



-- MODEL


type alias Model =
    Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | UpdateRepo ( String, Repo )
    | SaveRepo ( String, Repo )
    | DeleteRepo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateRepo ( key, repo ) ->
            ( Session.updateRepo key repo model, Cmd.none )

        SaveRepo ( key, repo ) ->
            ( Session.updateRepo key repo model, Session.store model )

        DeleteRepo key ->
            ( Session.removeRepo key model, Api.removeObject key )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Element Msg
view model =
    column
        [ width fill
        , height fill
        , padding 40
        , spacing 25
        , Font.size 12
        , Font.color <| white 0.95
        , Background.color <| Colors.black 0.8
        , scrollbarY
        , alignTop
        ]
        [ el [] <| el UI.Fonts.title <| text "Список репозиториев"
        , wrappedRow [ paddingXY 40 10 ] <| Dict.values <| Dict.map viewRepo model.repos
        ]


viewRepoControls : Element Msg
viewRepoControls =
    column
        [ alignTop, spacing 10 ]
        []


viewRepo : String -> Repo -> Element Msg
viewRepo key repo =
    row [ spacing 10, width shrink ] [ Repo.label key ]


viewEditableRepo : ( String, Repo ) -> Element Msg
viewEditableRepo ( key, repo ) =
    let
        inputStyle =
            [ width fill
            , spacing 5
            , focused [ Background.color <| lightGrey 0.8 ]
            , Event.onLoseFocus <| SaveRepo ( key, repo )
            ]
    in
    row
        [ spacing 10
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , width fill
        ]
        [ Input.text
            inputStyle
            { onChange = \new -> UpdateRepo ( key, { repo | name = new } )
            , text = repo.name
            , placeholder = Just <| Input.placeholder [] <| text "Имя репозитория"
            , label = Input.labelAbove [] <| none
            }
        , Input.text
            inputStyle
            { onChange = \new -> UpdateRepo ( key, { repo | name = new } )
            , text = repo.name
            , placeholder = Just <| Input.placeholder [] <| text "Имя репозитория"
            , label = Input.labelAbove [] <| none
            }
        , Input.text
            inputStyle
            { onChange = \new -> UpdateRepo ( key, { repo | description = new } )
            , text = repo.description
            , placeholder = Just <| Input.placeholder [] <| text "Описание репозитория"
            , label = Input.labelAbove [ Font.color <| orange 1.0, Font.size 11 ] <| validateText 50 repo.description
            }
        , Button.delete <| DeleteRepo key
        ]


validateText : Int -> String -> Element msg
validateText max str =
    let
        length =
            String.length str
    in
    if length > max then
        text <| "Допустимый размер описания превышен на " ++ String.fromInt (length - max) ++ " знаков"

    else
        none



-- HELPERS


cutHash : Maybe String -> String
cutHash maybe =
    case maybe of
        Just hash ->
            String.padLeft 9 '.' <| String.right 6 hash

        Nothing ->
            "пусто"
