module Session exposing (Session, Settings, default, getRepo, removeRepo, sessionDecoder, settingsDecoder, store, updateAuthor, updateRepo, updateSettings)

import Api
import Avatar exposing (Avatar)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Repo exposing (Author, Node, Repo)
import Route exposing (Path)
import Time
import Url exposing (Url)



-- TYPES


type alias Session =
    { url : Url
    , settings : Settings
    , repos : Dict String Repo
    }


type alias Settings =
    { shownodeprops : Bool
    , author : Author
    }



-- INIT


default : Url -> Session
default url =
    { url = url
    , repos = Repo.defaultRepos
    , settings = defaultSettings
    }


store : Session -> Cmd msg
store session =
    Api.storeObject ( "session", encodeSession session )


defaultSettings : Settings
defaultSettings =
    { author = { email = "default", name = "default" }
    , shownodeprops = False
    }



-- ENCODERS


encodeSettings : Settings -> Encode.Value
encodeSettings s =
    Encode.object
        [ ( "shownodeprops", Encode.bool s.shownodeprops )
        , ( "author", Repo.authorEncoder s.author )
        ]


encodeSession : Session -> Encode.Value
encodeSession session =
    Encode.object
        [ ( "url", Encode.string <| Url.toString session.url )
        , ( "repos", Repo.reposEncoder session.repos )
        , ( "settings", encodeSettings session.settings )
        ]



-- DECODERS


sessionDecoder : Url -> Decoder Session
sessionDecoder url =
    Decode.succeed Session
        |> hardcoded url
        |> required "settings" settingsDecoder
        |> required "repos" Repo.reposDecoder


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> required "shownodeprops" Decode.bool
        |> required "author" Repo.authorDecoder



-- GETTERS & SETTERS


getRepo : String -> Dict String Repo -> Maybe Repo
getRepo key dict =
    Dict.get key dict


removeRepo : String -> Session -> Session
removeRepo key session =
    { session | repos = Dict.remove key session.repos }


updateRepo : String -> Repo -> Session -> Session
updateRepo key repo session =
    { session | repos = Dict.insert key repo session.repos }


updateAuthor : Author -> Session -> Session
updateAuthor author session =
    let
        settings =
            session.settings
    in
    { session | settings = { settings | author = author } }


updateSettings : Settings -> Session -> Session
updateSettings settings session =
    { session | settings = settings }
