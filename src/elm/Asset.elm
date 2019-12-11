module Asset exposing (Image, defaultAvatar, dots, error, loading, logo, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}

import Element exposing (..)


type Image
    = Image String



-- IMAGES


error : Image
error =
    image "error.jpg"


loading : Image
loading =
    image "loading.svg"


dots : Image
dots =
    image "three-dots.svg"


defaultAvatar : Image
defaultAvatar =
    image "smiley-cyrus.jpg"


logo : Image
logo =
    image "suzdal_logo.svg"



--logo : Image
--logo =
--    Image "http://localhost:8080/ipfs/QmPiojQnt2vjPXJiD3rorSekeNA1rGcrpWpaFctvNkm7UB/suzdal_logo.svg"


image : String -> Image
image filename =
    Image ("images/" ++ filename)



-- USING IMAGES


src : Image -> String
src (Image url) =
    url
