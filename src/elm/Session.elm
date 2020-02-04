module Session exposing (Session, Settings, default, getRepo, removeRepo, sessionDecoder, settingsDecoder, store, updateAuthor, updateRepo, updateSettings)

import Api
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Repo exposing (Author, Repo)
import Url exposing (Url)



-- TYPES


type alias Session =
    { url : Url
    , settings : Settings
    , repos : Dict String Repo
    }


type alias Settings =
    { shownodeprops : Bool
    , animation : Bool
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
    , animation = False
    }



-- ENCODERS


encodeSettings : Settings -> Encode.Value
encodeSettings s =
    Encode.object
        [ ( "shownodeprops", Encode.bool s.shownodeprops )
        , ( "animation", Encode.bool s.animation )
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
        |> optional "shownodeprops" Decode.bool False
        |> optional "animation" Decode.bool False
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
