module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Browser.Dom as Dom
import Colors exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Loading
import Log
import Page
import PaginatedList exposing (PaginatedList)
import Route exposing (Path)
import Session exposing (Session)
import Task exposing (Task)
import Time
import Url.Builder
import Username exposing (Username)



-- MODEL


type alias Model =
    { session : Session
    , bookmarks : List Bookmark
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type alias Bookmark =
    { path : Path
    , description : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , bookmarks = []
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Домашняя страница"
    , content =
        column
            [ width fill
            , Font.size 12
            , padding 10
            , alignTop
            , padding 5
            , Font.color <| black 1.0
            ]
            [ el [ Font.size 20, padding 10 ] <| text "Домашняя страница"
            , column
                [ spacing 5
                , padding 2
                ]
              <|
                List.map viewBookMark model.bookmarks
            ]
    }


viewBookMark : Bookmark -> Element Msg
viewBookMark bookmark =
    link
        [ width (fill |> maximum 300)
        , Font.underline
        , padding 3
        , Border.width 1
        , Border.rounded 3
        ]
        { url = Route.pathToUrl bookmark.path
        , label = text bookmark.description
        }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


storeBookmarks : List Bookmark -> Cmd msg
storeBookmarks bookmarks =
    let
        json =
            bookmarksEncoder bookmarks
    in
    Api.setStorage (Just json)



-- SERIALISATION


bookmarksEncoder : List Bookmark -> Value
bookmarksEncoder bookmarks =
    let
        encoder mark =
            Encode.object
                [ ( "path", Api.pathEncoder mark.path )
                , ( "description", Encode.string mark.description )
                ]
    in
    Encode.object [ ( "bookmarks", Encode.list encoder bookmarks ) ]


bookmarksDecoder : Decoder (List Bookmark)
bookmarksDecoder =
    Decode.list bookmarkDecoder
        |> Decode.field "bookmarks"


bookmarkDecoder : Decoder Bookmark
bookmarkDecoder =
    Decode.succeed Bookmark
        |> required "path" Api.pathDecoder
        |> required "description" Decode.string
