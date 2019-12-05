module UI.Fonts exposing (title)

import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input
import Element.Lazy
import Html exposing (Html)
import Html.Attributes
import UI.Colors as Colors
import UI.Icons as Icons


title : List (E.Attribute msg)
title =
    [ Font.size 30 ]
