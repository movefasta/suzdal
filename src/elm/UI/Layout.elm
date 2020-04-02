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
import UI.Button as Button
import UI.Colors as Colors
import UI.Fonts as Fonts
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
        [ height fill
        , Font.family [ Font.typeface "Ubuntu-Regular" ]
        , Font.size 13
        , Font.letterSpacing 0.4
        ]
        content
    ]
