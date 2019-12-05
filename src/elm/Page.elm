module Page exposing (Page(..), view)

import Browser
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Repo
import Route exposing (Path, Route)
import Session exposing (Session)
import UI.Colors as Colors
import UI.Icons as Icons
import UI.Layout as Layout
import Url.Builder as Url


type Page
    = Other
    | Home
    | Repo String
    | Settings
    | Welcome



-- VIEW


view : Page -> { title : String, content : Element msg } -> Browser.Document msg
view page { title, content } =
    { title = title, body = Layout.toHtml <| row [ width fill, height fill ] [ viewMenu page, content ] }


viewMenu : Page -> Element msg
viewMenu page =
    let
        highlighted route =
            el
                [ if isActive page route then
                    E.alpha 1.0

                  else
                    E.alpha 0.25
                , mouseOver [ E.alpha 1.0 ]
                ]

        defaultRepos =
            List.map (\name -> highlighted (Route.Repo name) (Repo.menuLink name)) Repo.list
    in
    column
        [ height fill
        , width shrink
        , padding 10
        , spacing 10
        , Font.size 14
        , Background.color <| Colors.black 0.8
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 2
            , blur = 7
            , color = Colors.darkGrey 1.0
            }
        ]
    <|
        List.concat
            [ [ highlighted Route.Home <| homeLink ]
            , defaultRepos
            , [ highlighted Route.Settings <| settingsLink ]
            ]


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        ( Repo reponame, Route.Repo name ) ->
            name == reponame

        _ ->
            False


homeLink : Element msg
homeLink =
    menuLink "home" "Домашняя страница" "White" Icons.home


settingsLink : Element msg
settingsLink =
    menuLink "settings" "Настройки" "White" Icons.settings


menuLink : String -> String -> String -> Html msg -> Element msg
menuLink key title color icon =
    el [ htmlAttribute <| Html.Attributes.style "color" color ] <|
        link
            [ htmlAttribute <| Html.Attributes.title title
            ]
            { url = Url.relative [ "#", key ] []
            , label = el [ width <| px 30 ] <| html icon
            }


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
