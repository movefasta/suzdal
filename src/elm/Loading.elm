module Loading exposing (error, icon, slowThreshold)

{-| A loading spinner icon.
-}

import Asset
import Element exposing (..)
import Process
import Task exposing (Task)


icon : String -> Element msg
icon desc =
    image [ width <| px 64, height <| px 64 ] { src = "../" ++ Asset.src Asset.loading, description = desc }


error : String -> Element msg
error str =
    text ("Error loading " ++ str ++ ".")


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 200
