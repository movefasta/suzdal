module Page exposing (Page(..), processPath, view)

import Api
import Avatar
import Browser exposing (Document)
import Colors exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2)
import Html exposing (Html)
import Html.Attributes
import Icons
import Json.Decode as Decode
import Route exposing (Path, Route)
import Session exposing (Session)
import Username exposing (Username)


type Page
    = Other
    | Home
    | Tensor
    | Settings



-- VIEW


view : Maybe Path -> Page -> { title : String, content : Element msg } -> Browser.Document msg
view maybepath page { title, content } =
    { title = title ++ " - Трактовочная сеть"
    , body =
        [ layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ height fill ]
          <|
            row
                [ width fill
                , height fill
                ]
            <|
                case maybepath of
                    Just path ->
                        viewMenu path page :: [ content ]

                    Nothing ->
                        [ content ]

        -- el [ centerX, centerY ] <| html Icons.loader
        ]
    }


viewMenu : Path -> Page -> Element msg
viewMenu path page =
    let
        linkTo =
            navbarLink page
    in
    column
        [ height fill
        , width shrink
        , padding 10
        , spacing 10
        , Font.size 14
        , Background.color <| black 0.8

        --, Border.color <| black 0.8
        --, Border.widthEach { edges | right = 3 }
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 2
            , blur = 7
            , color = darkGrey 1.0
            }
        ]
        [ linkTo Route.Home
        , linkTo <| Route.Tensor path
        , linkTo Route.Settings
        ]


navbarLink : Page -> Route -> Element msg
navbarLink page route =
    link
        [ htmlAttribute <|
            Html.Attributes.style "color" <|
                if isActive page route then
                    "orange"

                else
                    "white"
        ]
        { url = Route.routeToString route
        , label =
            html <|
                case route of
                    Route.Home ->
                        Icons.home

                    Route.Tensor path ->
                        Icons.grid

                    Route.Settings ->
                        Icons.settings
        }


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Tensor, Route.Tensor _ ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        _ ->
            False



-- helper function - convert single path [0,1,2] to list of paths [[0], [0,1], [0,1,2]]


processPath : Path -> (Path -> a) -> List a -> List a
processPath path fun acc =
    case path.location of
        x :: xs ->
            processPath
                { path | location = xs }
                fun
                ([ fun { path | location = List.reverse (x :: xs) } ] ++ acc)

        [] ->
            acc


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }



{-
   viewPath : Path -> List (Element msg) -> List (Element msg)
   viewPath path acc =
       let
           crumb p =
               link
                   [ width <| px 20
                   , height <| px 20
                   , spacing 3
                   , Border.rounded 10
                   ]
                   { url = Route.pathToUrl p
                   , label = text <| Maybe.withDefault "T" <| List.head <| List.reverse <| List.map String.fromInt p.location
                   }
       in
       processPath path crumb []


      Render dismissable errors. We use this all over the place!
      viewErrors : msg -> List String -> Html msg
      viewErrors dismissErrors errors =
      if List.isEmpty errors then
      Html.text ""

          else
              div
                  [ class "error-messages"
                  , style "position" "fixed"
                  , style "top" "0"
                  , style "background" "rgb(250, 250, 250)"
                  , style "padding" "20px"
                  , style "border" "1px solid"
                  ]
              <|
                  List.map (\error -> p [] [ text error ]) errors
                      ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]

-}
