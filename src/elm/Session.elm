module Session exposing (Session, fromPath, init, navKey, path, pathDecoder, update, url)

import Api
import Avatar exposing (Avatar)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Route exposing (Path)
import Time
import Url exposing (Url)



-- TYPES


type Session
    = Session Nav.Key Path Url
    | Init Nav.Key Url



-- INFO


init : Url -> Nav.Key -> Session
init str key =
    Init key str


update : Session -> Path -> Url -> Session
update session newpath str =
    case session of
        Session key _ _ ->
            Session key newpath str

        Init key _ ->
            Session key newpath str


path : Session -> Maybe Path
path session =
    case session of
        Session _ val _ ->
            Just val

        Init _ _ ->
            Nothing


url : Session -> Url
url session =
    case session of
        Session _ _ a ->
            a

        Init _ a ->
            a


navKey : Session -> Nav.Key
navKey session =
    case session of
        Session key _ _ ->
            key

        Init key _ ->
            key



-- SERIALIZATION


pathDecoder : Decoder Path
pathDecoder =
    Decode.succeed Path
        |> requiredAt [ "path", "cid" ] Decode.string
        |> requiredAt [ "path", "location" ] (Decode.list Decode.int)



-- CHANGES
--changes : (Session -> msg) -> Nav.Key -> Sub msg
--changes toMsg key =
--    Api.viewerChanges (\x -> toMsg (fromPath key x)) pathDecoder


fromPath : Nav.Key -> Path -> Url -> Session
fromPath key p str =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Session key p str
