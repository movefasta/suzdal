module UI.Layout exposing (toHtml)

import Browser
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy exposing (lazy)
import Html exposing (Html)
import Html.Attributes
import Html.Events as HtmlEvents
import UI.Colors as Colors
import UI.Icons as Icons


toHtml : Element msg -> List (Html msg)
toHtml content =
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
        content
    ]
