module UI.Colors exposing
    ( black
    , blue
    , cyan
    , darkGrey
    , fromHsl
    , green
    , lightGrey
    , lime
    , orange
    , red
    , violet
    , white
    , yellow
    )

import Element exposing (Color, rgba, rgba255)



--, Input.radioRow
--    [ width fill
--    , spacing 5
--    ]
--    { onChange = \new -> UpdateFocus { node | color = new }
--    , selected = Just node.color
--    , label = Input.labelAbove [ padding 3 ] (el [ Font.size 10 ] <| text "Цвет")
--    , options =
--        let
--            option i x =
--                el
--                    [ width <| px 30
--                    , height <| px 25
--                    , Border.widthEach { bottom = 3, left = 0, right = 0, top = 0 }
--                    , Background.color <| colorCodeConverter i hue 1.0
--                    , Border.color <|
--                        case x of
--                            Input.Idle ->
--                                white 0
--                            Input.Focused ->
--                                lightGrey 0.8
--                            Input.Selected ->
--                                darkGrey 1.0
--                    ]
--                <|
--                    text ""
--        in
--        List.range 0 9
--            |> List.map
--                (\code ->
--                    Input.optionWith code (option code)
--                )
--    }
-- FROM HSL


fromHsl : Float -> Float -> Float -> Float -> Color
fromHsl hue saturation lightness alpha =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        normHue =
            hue / degrees 60

        x =
            chroma * (1 - abs (modFloatBy 2 normHue - 1))

        m =
            lightness - chroma / 2

        make r g b =
            rgba (r + m) (g + m) (b + m) alpha
    in
    if normHue < 0 then
        make 0 0 0

    else if normHue < 1 then
        make chroma x 0

    else if normHue < 2 then
        make x chroma 0

    else if normHue < 3 then
        make 0 chroma x

    else if normHue < 4 then
        make 0 x chroma

    else if normHue < 5 then
        make x 0 chroma

    else if normHue < 6 then
        make chroma 0 x

    else
        make 0 0 0


modFloatBy : Int -> Float -> Float
modFloatBy n float =
    let
        integer =
            floor float
    in
    toFloat (modBy n integer) + float - toFloat integer



-- TO HSL


type alias Hsl =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


toHsl : Float -> Float -> Float -> Float -> Hsl
toHsl r g b a =
    let
        cMax =
            max (max r g) b

        cMin =
            min (min r g) b

        c =
            cMax - cMin

        hue =
            degrees 60
                * (if cMax == r then
                    modFloatBy 6 ((g - b) / c)

                   else if cMax == g then
                    ((b - r) / c) + 2

                   else
                    {- cMax == b -}
                    ((r - g) / c) + 4
                  )

        lightness =
            (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    { hue = hue
    , saturation = saturation
    , lightness = lightness
    , alpha = a
    }


toColor : Float -> Float -> Float -> Float -> Color
toColor r g b a =
    rgba r g b a



-- COLOR PROPERTIES


white : Float -> Color
white alpha =
    rgba255 255 255 255 alpha


orange : Float -> Color
orange alpha =
    rgba255 255 122 0 alpha


red : Float -> Color
red alpha =
    rgba255 255 187 187 alpha


yellow : Float -> Color
yellow alpha =
    rgba255 255 255 187 alpha


green : Float -> Color
green alpha =
    rgba255 187 255 187 alpha


lime : Float -> Color
lime alpha =
    rgba255 152 237 0 alpha


cyan : Float -> Color
cyan alpha =
    rgba255 187 255 255 alpha


blue : Float -> Color
blue alpha =
    rgba255 187 187 255 alpha


violet : Float -> Color
violet alpha =
    rgba255 255 187 255 alpha


lightGrey : Float -> Color
lightGrey alpha =
    rgba255 211 211 211 alpha


darkGrey : Float -> Color
darkGrey alpha =
    rgba255 169 169 169 alpha


black : Float -> Color
black alpha =
    rgba255 0 0 0 alpha
