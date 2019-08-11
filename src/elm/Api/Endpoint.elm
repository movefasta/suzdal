module Api.Endpoint exposing (Endpoint, add, config, configSet, connect, content, dagGet, dagPut, file, getContent, id, publish, request, resolve, swarmPeers, task, unwrap)

import CommentId exposing (CommentId)
import Http
import Json.Encode as Encode
import Route exposing (Path)
import Task
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)



-- ENDPOINTS


getContent : Url -> String -> Endpoint
getContent url cid =
    urlBuilder (gateway url) [ cid ] []


file : Url -> String -> String
file url cid =
    unwrap (getContent url cid)


dagGet : Url -> String -> Endpoint
dagGet url cid =
    urlBuilder (endpoint url) [ "dag", "get" ] [ Url.Builder.string "arg" cid ]


dagPut : Url -> Endpoint
dagPut url =
    urlBuilder (endpoint url) [ "dag", "put" ] [ Url.Builder.string "pin" "true" ]


publish : Url -> String -> Endpoint
publish url cid =
    urlBuilder (endpoint url) [ "name", "publish" ] [ Url.Builder.string "arg" cid, Url.Builder.string "allow-offline" "true" ]



--connect : Url -> String -> String -> Endpoint
--connect url peerid relay =
--    urlBuilder (endpoint url) [ "swarm", "connect" ] [ Url.Builder.string "arg" ("/ipfs/" ++ relay ++ "/p2p-circuit/ipfs/" ++ peerid) ]


connect : Url -> String -> String -> Endpoint
connect url peerid relay =
    endpoint url
        ++ "/swarm/connect?arg="
        --++ "/ipfs/"
        --++ relay
        ++ "/p2p-circuit/ipfs/"
        ++ peerid
        |> Endpoint


resolve : Url -> String -> Endpoint
resolve url peerid =
    urlBuilder (endpoint url) [ "name", "resolve" ] [ Url.Builder.string "arg" peerid ]


content : Url -> Route.Path -> Endpoint
content url path =
    let
        location =
            List.map (\s -> "links/" ++ String.fromInt s) path.location
                |> String.join "/"
    in
    endpoint url
        ++ "/dag/get?arg="
        ++ path.cid
        ++ "/"
        ++ location
        ++ "/cid"
        |> Endpoint


config : Url -> Endpoint
config url =
    urlBuilder (endpoint url) [ "config", "show" ] []


swarmPeers : Url -> Endpoint
swarmPeers url =
    urlBuilder (endpoint url) [ "swarm", "peers" ] []


configSet : Url -> String -> Encode.Value -> Endpoint
configSet url key value =
    urlBuilder (endpoint url) [ "config" ] [ Url.Builder.string "arg" key, Url.Builder.string "arg" <| Encode.encode 0 value ]


id : Url -> Endpoint
id url =
    urlBuilder (endpoint url) [ "id" ] []


add : Url -> Endpoint
add url =
    urlBuilder (endpoint url) [ "add" ] [ Url.Builder.string "pin" "true" ]



-- TYPES


type Endpoint
    = Endpoint String


urlBuilder : String -> List String -> List QueryParameter -> Endpoint
urlBuilder host paths queryParams =
    Url.Builder.crossOrigin
        host
        paths
        queryParams
        |> Endpoint


endpoint : Url -> String
endpoint url =
    Url.toString { url | port_ = Just 5001, path = "/api/v0", fragment = Nothing }


gateway : Url -> String
gateway url =
    Url.toString { url | port_ = Just 8080, path = "/ipfs", fragment = Nothing }


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request r =
    Http.request
        { method = r.method
        , headers = r.headers
        , url = unwrap r.url
        , body = r.body
        , expect = r.expect
        , timeout = r.timeout
        , tracker = r.tracker
        }


task :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , resolver : Http.Resolver x a
    , timeout : Maybe Float
    }
    -> Task.Task x a
task r =
    Http.task
        { method = r.method
        , headers = r.headers
        , url = unwrap r.url
        , body = r.body
        , resolver = r.resolver
        , timeout = r.timeout
        }
