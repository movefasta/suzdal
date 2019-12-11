module Loading exposing (dots, error, failed, icon, logo, slowThreshold, spinner)

{-| A loading spinner icon.
-}

import Asset exposing (Image)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Process
import Task exposing (Task)
import UI.Colors exposing (..)
import UI.Icons as Icons


icon : Int -> String -> Image -> Element msg
icon size desc asset =
    column
        [ spacing 40, centerX, centerY ]
        [ image [ centerX, width <| px size, height <| px size ] { src = Asset.src asset, description = "" }
        , el [ centerX ] <| text desc
        ]


logo : Int -> Element msg
logo size =
    image [ centerX, width <| px size, height <| px size ] { src = Asset.src Asset.logo, description = "" }


error : String -> Element msg
error str =
    text ("Error loading " ++ str ++ ".")


spinner : String -> Element msg
spinner str =
    icon 64 str Asset.loading


dots : Bool -> Element msg
dots ifLoading =
    image
        [ transparent <| not ifLoading
        , centerX
        , width <| px 20
        , height <| px 20
        , alignBottom
        , alignRight
        , Font.size 10
        , Font.color <| white 1.0
        , moveLeft 5
        , moveDown 2
        ]
        { src = Asset.src Asset.dots
        , description = ""
        }


failed : String -> Element msg
failed str =
    el [ centerX, centerY, width fill, height fill ] <|
        column [ spacing 40, centerX, centerY ]
            [ el
                [ centerX
                , htmlAttribute <| Html.Attributes.style "color" "OrangeRed"
                , width <| px 64
                , height <| px 64
                ]
              <|
                html Icons.alertCircle
            , el [ centerX ] <| text str
            ]


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 200
