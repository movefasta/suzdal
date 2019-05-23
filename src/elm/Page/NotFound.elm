module Page.NotFound exposing (view)

import Asset
import Element as E exposing (..)



-- VIEW


view : { title : String, content : Element msg }
view =
    { title = "Страница не найдена"
    , content =
        paragraph
            [ width <| px 500, centerY, centerX ]
            [ text "Страница с таким адресом не найдена. Попробуйте нажать слева на одну из иконок навигации" ]
    }
