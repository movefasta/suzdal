module Api.Endpoint exposing (Endpoint, add, changeLog, cluster, config, configSet, connect, content, dagGet, dagPut, file, filesLs, filesRead, filesWrite, getContent, getInitRepos, id, links, node, objectGet, pinAdd, pinLs, pinUpdate, pinVerify, publish, repoStat, request, resolve, swarmPeers, task, unwrap)

import Http
import Json.Encode as Encode
import Route
import Task
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)



-- ENDPOINTS


changeLog : Endpoint
changeLog =
    Url.Builder.relative [ "CHANGELOG.md" ] []
        |> Endpoint


getInitRepos : Endpoint
getInitRepos =
    Url.Builder.relative [ "repos.json" ] []
        |> Endpoint


getContent : Url -> String -> Endpoint
getContent url cid =
    urlBuilder (gateway url) [ cid ] []


file : Url -> String -> String
file url cid =
    unwrap (getContent url cid)


dagGet : Url -> String -> Endpoint
dagGet url cid =
    urlBuilder (endpoint url) [ "dag", "get" ] [ Url.Builder.string "arg" cid ]


objectGet : Url -> String -> Endpoint
objectGet url cid =
    urlBuilder (endpoint url) [ "object", "get" ] [ Url.Builder.string "arg" cid ]


dagPut : Url -> Endpoint
dagPut url =
    urlBuilder (endpoint url) [ "dag", "put" ] []


publish : Url -> String -> Endpoint
publish url cid =
    urlBuilder (endpoint url) [ "name", "publish" ] [ Url.Builder.string "arg" cid, Url.Builder.string "allow-offline" "true" ]


filesRead : Url -> String -> Endpoint
filesRead url path =
    urlBuilder (endpoint url) [ "files", "read" ] [ Url.Builder.string "arg" path ]


filesWrite : Url -> String -> Endpoint
filesWrite url path =
    let
        options =
            String.join "&" [ path, "create=true", "truncate=true", "parents=true" ]
    in
    Endpoint (endpoint url ++ "/files/write?arg=" ++ options)


filesLs : Url -> String -> Endpoint
filesLs url path =
    Endpoint (endpoint url ++ "/files/ls?arg=" ++ path ++ "&" ++ "l=true")


filesStat : Url -> String -> Endpoint
filesStat url path =
    urlBuilder (endpoint url) [ "files", "stat" ] [ Url.Builder.string "arg" path ]



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


links : Url -> Route.Path -> Endpoint
links url path =
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
        ++ "/links"
        |> Endpoint


node : Url -> Route.Path -> Endpoint
node url path =
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


pinAdd : Url -> String -> Endpoint
pinAdd url hash =
    urlBuilder (endpoint url) [ "pin", "add" ] [ Url.Builder.string "arg" hash, Url.Builder.string "recursive" "true" ]


add : Url -> Endpoint
add url =
    urlBuilder (endpoint url) [ "add" ] [ Url.Builder.string "pin" "false" ]


repoStat : Url -> Endpoint
repoStat url =
    urlBuilder (endpoint url) [ "repo", "stat" ] []


pinLs : Url -> Maybe String -> Endpoint
pinLs url maybe_hash =
    let
        arg =
            case maybe_hash of
                Just hash ->
                    [ Url.Builder.string "arg" hash ]

                Nothing ->
                    []
    in
    urlBuilder (endpoint url) [ "pin", "ls" ] <| Url.Builder.string "type" "recursive" :: arg


pinRm : Url -> String -> Endpoint
pinRm url hash =
    urlBuilder (endpoint url) [ "pin", "rm" ] [ Url.Builder.string "arg" hash ]


pinUpdate : Url -> String -> String -> Endpoint
pinUpdate url old_hash new_hash =
    urlBuilder (endpoint url)
        [ "pin", "update" ]
        [ Url.Builder.string "arg" old_hash
        , Url.Builder.string "arg" new_hash
        , Url.Builder.string "unpin" "true"
        ]


pinVerify : Url -> String -> Endpoint
pinVerify url hash =
    urlBuilder (endpoint url) [ "pin", "verify" ] []



-- pin ls with arg SUCCESS - {"Keys":{"QmHash":{"Type":"recursive"}}}
-- pin ls with arg NOT FOUND - {"Message":"merkledag: not found","Code":0,"Type":"error"}
-- pin ls with arg INVALID PATH - {"Message":"invalid path \"QmHash\": multihash length inconsistent: \u0026{18 sha2-256 32 [93 ... 241]}","Code":0,"Type":"error"}
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


cluster : Url -> String
cluster url =
    Url.toString { url | port_ = Just 9094, path = "", fragment = Nothing }


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
