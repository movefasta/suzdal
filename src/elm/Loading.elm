module Loading exposing (error, failed, icon, logo, slowThreshold, spinner)

{-| A loading spinner icon.
-}

import Asset
import Element exposing (..)
import Html.Attributes
import Process
import Task exposing (Task)
import UI.Icons as Icons


icon : Int -> String -> Element msg
icon size desc =
    column
        [ spacing 40, centerX, centerY ]
        [ image [ centerX, width <| px size, height <| px size ] { src = Asset.src Asset.loading, description = "" }
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
    icon 64 str


failed : String -> Element msg
failed str =
    el [ centerX, centerY, width fill, height fill ] <|
        column [ spacing 40, centerX, centerY ]
            [ el [ centerX, htmlAttribute <| Html.Attributes.style "color" "OrangeRed" ] <| html Icons.alertCircle
            , el [ centerX ] <| text str
            ]


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 200
