port module Api exposing (Hash, addServerError, cidDecoder, decodeErrors, get, jsonToHttpBody, objectRetrieved, pathEncoder, post, put, removeObject, retrieveObject, settings, storeObject, storePeers, task)

{-| This module is responsible for communicating to the IPFS API.
-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Bytes.Encode
import Http exposing (Body)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, string)
import Json.Encode as Encode
import Result exposing (Result)
import Route exposing (Path)
import Task
import Url exposing (Url)


type alias Hash =
    String



-- PORTS


port storeObject : ( String, Value ) -> Cmd msg


port retrieveObject : String -> Cmd msg


port objectRetrieved : (( String, Value ) -> msg) -> Sub msg


port removeObject : String -> Cmd msg



-- SERIALIZATION


pathEncoder : Path -> Value
pathEncoder path =
    Encode.object
        [ ( "path"
          , Encode.object
                [ ( "cid", Encode.string path.cid )
                , ( "location", Encode.list Encode.int path.location )
                ]
          )
        ]



-- HTTP


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url trigger decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson trigger decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> Body -> (Result Http.Error Hash -> msg) -> Cmd msg
post url body trigger =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson trigger cidDecoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


task : String -> Endpoint -> Http.Body -> Decode.Decoder a -> Task.Task Http.Error a
task method url body decoder =
    Endpoint.task
        { method = method
        , headers = []
        , url = url
        , body = body
        , resolver = Http.stringResolver <| expect decoder
        , timeout = Nothing
        }


put : Url -> Value -> (Result Http.Error Hash -> msg) -> Cmd msg
put url value trigger =
    post (Endpoint.dagPut url) (jsonToHttpBody value) trigger


settings : Url -> Http.Body -> (Result Http.Error Hash -> msg) -> Cmd msg
settings url body trigger =
    post (Endpoint.dagPut url) body trigger



-- ERRORS


addServerError : List String -> List String
addServerError list =
    "Server error" :: list



-- Request Helpers


expect : Decode.Decoder a -> Http.Response String -> Result Http.Error a
expect decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))


decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        Http.BadStatus _ ->
            [ "Server error" ]

        _ ->
            [ "Server error" ]


cidDecoder : Decode.Decoder Hash
cidDecoder =
    Decode.at [ "Cid", "/" ] Decode.string


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors


jsonToHttpBody : Value -> Http.Body
jsonToHttpBody value =
    Http.multipartBody
        [ turnToBytesPart "whatever" "application/json" value ]


turnToBytesPart : String -> String -> Encode.Value -> Http.Part
turnToBytesPart message mime json =
    Encode.encode 0 json
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Http.bytesPart message mime



-- LOCALSTORAGE FUNCTIONS


storePeers : Encode.Value -> Cmd msg
storePeers value =
    storeObject ( peersStorageKey, value )



-- LOCALSTORAGE KEYS


peersStorageKey : String
peersStorageKey =
    "peers"



-- PORTS HELPERS
{-
   recieveLocalStorageObject : (a -> msg) -> Decoder a -> a -> Sub msg
   recieveLocalStorageObject msg dec default =
       let
           result json =
               case Decode.decodeValue dec json of
                   Ok m ->
                       m

                   Err _ ->
                       default

           retrieval ( _, json ) =
               msg (result json)
       in
       objectRetrieved retrieval
-}
