module Colors exposing (black, blue, cyan, darkGrey, green, lightGrey, orange, violet, white, yellow)

import Element exposing (Color, rgba255)



-- COLOR PROPERTIES


white : Float -> Color
white alpha =
    rgba255 255 255 255 alpha


orange : Float -> Color
orange alpha =
    rgba255 255 122 0 alpha


yellow : Float -> Color
yellow alpha =
    rgba255 255 214 0 alpha


green : Float -> Color
green alpha =
    rgba255 152 237 0 alpha


cyan : Float -> Color
cyan alpha =
    rgba255 2 142 155 alpha


blue : Float -> Color
blue alpha =
    rgba255 62 19 175 alpha


violet : Float -> Color
violet alpha =
    rgba255 210 0 107 alpha


lightGrey : Float -> Color
lightGrey alpha =
    rgba255 211 211 211 alpha


darkGrey : Float -> Color
darkGrey alpha =
    rgba255 169 169 169 alpha


black : Float -> Color
black alpha =
    rgba255 0 0 0 alpha
