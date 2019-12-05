module Route exposing (Path, Route(..), fromUrl, locationToString, pathToUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Settings
    | Repo String
    | Welcome


type alias Path =
    { cid : String
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


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home (s "home")
        , Parser.map Settings (s "settings")
        , Parser.map Repo (s "repo" </> string)
        , Parser.map Welcome (s "welcome")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



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

                Settings ->
                    [ "settings" ]

                Repo key ->
                    [ "repo", key ]

                Welcome ->
                    [ "welcome" ]
    in
    "#/" ++ String.join "/" pieces



{-
   ex_fromUrl : Url -> Maybe Route
   ex_fromUrl url =
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
                       Just <| Repo { cid = str, location = location }

           Nothing ->
               Nothing

-}
