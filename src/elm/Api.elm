port module Api exposing (Hash, addServerError, application, check, cidDecoder, decodeErrors, get, pathDecoder, pathEncoder, post, put, settings, storeCache, storeSettings, task, viewerChanges)

{-| This module is responsible for communicating to the IPFS API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Avatar exposing (Avatar)
import Browser
import Browser.Navigation as Nav
import Bytes
import Bytes.Encode
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required, requiredAt)
import Json.Encode as Encode
import Result exposing (Result)
import Route exposing (Path)
import Task
import Url exposing (Url)
import Username exposing (Username)


type alias Hash =
    String



-- PERSISTENCE


cidDecoder : Decode.Decoder Hash
cidDecoder =
    Decode.at [ "Cid", "/" ] Decode.string


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Path -> msg) -> Decoder Path -> Sub msg
viewerChanges toMsg decoder =
    let
        decodeFromChange dec val =
            case Decode.decodeValue dec val of
                Ok path ->
                    path

                Err _ ->
                    { cid = "", location = [] }
    in
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


storeSettings : Path -> Cmd msg
storeSettings path =
    let
        json =
            pathEncoder path
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION


pathDecoder : Decoder Path
pathDecoder =
    Decode.succeed Path
        |> requiredAt [ "path", "cid" ] Decode.string
        |> requiredAt [ "path", "location" ] (Decode.list Decode.int)


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



-- APPLICATION


application :
    Decoder Path
    ->
        { init : Maybe Path -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application dec config =
    let
        init flags url navKey =
            case decode dec flags of
                Ok path ->
                    config.init (Just path) url navKey

                Err _ ->
                    config.init Nothing url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


decode : Decoder Path -> Value -> Result Decode.Error Path
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString decoder str)



-- HTTP


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url trigger decoder =
    Endpoint.request
        { method = "GET"
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


check : Url -> Task.Task Http.Error Hash
check url =
    let
        node hash =
            Encode.object
                [ ( "cid", Encode.object [ ( "/", Encode.string hash ) ] )
                , ( "description", Encode.string "Кругозор" )
                , ( "links", Encode.object [ ( "/", Encode.string hash ) ] )
                , ( "name", Encode.string "0" )
                , ( "size", Encode.int 0 )
                , ( "color", Encode.int 0 )
                ]
                |> jsonToBody

        emptyListBody =
            Encode.list Encode.int []
                |> jsonToBody
    in
    task "POST" (Endpoint.dagPut url) emptyListBody cidDecoder
        |> Task.andThen (\cid -> task "POST" (Endpoint.dagPut url) (node cid) cidDecoder)


put : Url -> Value -> (Result Http.Error Hash -> msg) -> Cmd msg
put url value trigger =
    post (Endpoint.dagPut url) (jsonToBody value) trigger


jsonToBody : Value -> Http.Body
jsonToBody value =
    Http.multipartBody
        [ turnToBytesPart "whatever" "application/json" value ]


settings : Url -> Http.Body -> (Result Http.Error Hash -> msg) -> Cmd msg
settings url body trigger =
    post (Endpoint.dagPut url) body trigger


turnToBytesPart : String -> String -> Encode.Value -> Http.Part
turnToBytesPart message mime json =
    Encode.encode 0 json
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Http.bytesPart message mime



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

        err ->
            [ "Server error" ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


urlStorageKey : String
urlStorageKey =
    "url"


settingsStorageKey : String
settingsStorageKey =
    "settings"


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"
