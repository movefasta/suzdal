module Route exposing (Path, Route(..), fromUrl, href, locationToString, pathToUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)



-- МАРШРУТИЗАТОР
-- общий путь - этап/строение/уровень/тензор/сектор/строка/клетка
-- кругозор состоит из 6 этапов
-- этап (phase) состоит из 1-6 строений
-- строение (structure) состоит из 6 уровней
-- уровень (level) состоит из 1, 2, 3, 4, 6 таблиц
-- тензор (tensor) состоит из 8 или 12 секторов
-- сектор (sector) состоит из 1-7 строк
-- строка (row) состоит из 2-7 клеток (cell)
-- центральная клетка сектора ссылается на другой сектор
-- ROUTING


type Route
    = Home
    | Tensor Path
    | Settings


type alias Cid =
    String


type alias Path =
    { cid : Cid
    , location : List Int
    }


pathToUrl : Path -> String
pathToUrl path =
    let
        location =
            List.map String.fromInt path.location
    in
    String.join "/" ("#" :: path.cid :: location)


locationToString : String -> List Int -> String
locationToString separator ints =
    List.map String.fromInt ints
        |> String.join separator



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    let
        u =
            { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }

        pathlist =
            preparePath u.path

        location =
            List.tail pathlist
                |> Maybe.withDefault []
                |> List.map (Maybe.withDefault 99 << String.toInt)
    in
    case List.head pathlist of
        Just str ->
            case str of
                "home" ->
                    Just Home

                "settings" ->
                    Just Settings

                _ ->
                    Just <| Tensor { cid = str, location = location }

        Nothing ->
            Nothing



-- PREPARE PATH


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        segment :: rest ->
            segment :: removeFinalEmpty rest



-- INTERNAL


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    [ "home" ]

                Tensor path ->
                    [ path.cid ] ++ List.map String.fromInt path.location

                Settings ->
                    [ "settings" ]
    in
    "#/" ++ String.join "/" pieces
