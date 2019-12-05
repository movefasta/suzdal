module Page.Blank exposing (view)

import Element exposing (..)


view : { title : String, content : Element msg }
view =
    { title = ""
    , content = text ""
    }
