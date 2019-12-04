module Content exposing (Content, Link, Status(..), addText, contentEncoder, contentSize, fetch, fetchByCid, pickFiles, update, uploadText)

import Api exposing (Hash, jsonToHttpBody)
import Api.Endpoint as Endpoint exposing (Endpoint)
import Bytes
import Bytes.Encode
import File exposing (File)
import File.Download
import File.Select
import Http
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra exposing (parseInt)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import MimeType as Mime
import Route exposing (Path)
import Task exposing (Task)
import Url exposing (Url)



-- MODEL


type alias Content =
    List Link


type alias Link =
    { id : Int
    , status : Status
    , name : String
    , cid : Hash
    , size : Int
    , description : String
    , mimetype : Maybe Mime.MimeType
    }


type alias IpfsLink =
    { name : String
    , hash : String
    , size : Int
    }


type alias Object =
    { links : List IpfsLink
    , data : String
    }


type alias ObjectStat =
    { hash : String
    , numLinks : Int
    , blockSize : Int
    , linksSize : Int
    , dataSize : Int
    , cumulativeSize : Int
    }


type Status
    = Editing
    | Uploading Float
    | Completed
    | UploadingFail
    | Selected
    | Waiting
    | Changed
    | GotProgress



-- HELPERS


contentSize : Content -> Int
contentSize content =
    List.map .size content
        |> List.sum


pickFiles : (File -> List File -> msg) -> Cmd msg
pickFiles msg =
    File.Select.files [ "*" ] msg


addText : Content -> Content
addText content =
    List.indexedMap (\i a -> { a | id = i }) <| [ initText ] ++ content


initText : Link
initText =
    { id = 0
    , status = Editing
    , name = ""
    , cid = ""
    , size = 0
    , description = ""
    , mimetype = Just (Mime.Text Mime.PlainText)
    }



-- REQUESTS


uploadText : Endpoint -> Int -> String -> Task Http.Error Link
uploadText endpoint id string =
    let
        bytes =
            string
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        body =
            Http.multipartBody [ Http.bytesPart "whatever" "text/plain" bytes ]
    in
    objLinkDecoder
        |> Decode.andThen linkToNodeDecoder
        |> Api.task "POST" endpoint body
        |> Task.andThen
            (\link ->
                Task.succeed
                    { link
                        | mimetype = Mime.parseMimeType "text/plain"
                        , size = Bytes.width bytes
                        , description =
                            if Bytes.width bytes < 5000 then
                                string

                            else
                                ""
                        , status = Completed
                        , id = id
                        , cid = link.cid
                    }
            )


uploadFile : Endpoint -> File -> Task Http.Error Link
uploadFile endpoint file =
    let
        body =
            Http.multipartBody [ Http.filePart "file" file ]

        decoder =
            objLinkDecoder
                |> Decode.andThen linkToNodeDecoder
    in
    Api.task "POST" endpoint body decoder
        |> Task.andThen (\x -> Task.succeed { x | mimetype = File.mime file |> Mime.parseMimeType })


update : Url -> Content -> List File -> Task Http.Error String
update url content newfiles =
    List.map (uploadFile <| Endpoint.add url) newfiles
        |> Task.sequence
        |> Task.andThen
            (\list ->
                let
                    body =
                        List.indexedMap (\i a -> { a | id = i }) (list ++ content)
                            |> contentEncoder
                            |> jsonToHttpBody
                in
                Api.task "POST" (Endpoint.dagPut url) body (Decode.at [ "Cid", "/" ] Decode.string)
            )


fetch : Url -> Path -> (Result Http.Error Content -> msg) -> Cmd msg
fetch url path msg =
    Api.get (Endpoint.content url path) msg contentDecoder


fetchByCid : Url -> String -> (Result Http.Error Content -> msg) -> Cmd msg
fetchByCid url cid msg =
    Api.get (Endpoint.dagGet url cid) msg contentDecoder



-- DECODERS


objLinkDecoder : Decode.Decoder IpfsLink
objLinkDecoder =
    Decode.succeed IpfsLink
        |> required "Name" Decode.string
        |> required "Hash" Decode.string
        |> required "Size" sizeDecoder


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list objLinkDecoder)
        |> required "Data" Decode.string


linkToNodeDecoder : IpfsLink -> Decode.Decoder Link
linkToNodeDecoder link =
    Decode.succeed Link
        |> hardcoded 0
        |> hardcoded Completed
        |> hardcoded link.name
        |> hardcoded link.hash
        |> hardcoded link.size
        |> optional "description" Decode.string ""
        |> optional "mimetype" (Decode.map Mime.parseMimeType Decode.string) Nothing


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.list linkDecoder


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.succeed Link
        |> hardcoded 0
        |> hardcoded Completed
        |> optional "name" Decode.string "0"
        |> required "cid" (Decode.field "/" Decode.string)
        |> optional "size" sizeDecoder 0
        |> optional "description" Decode.string ""
        |> required "mimetype" (Decode.map Mime.parseMimeType Decode.string)


sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf [ Decode.int, DecodeExtra.parseInt ]



-- ENCODERS


linkEncoder : Link -> Encode.Value
linkEncoder link =
    Encode.object
        [ ( "name", Encode.string link.name )
        , ( "size", Encode.int link.size )
        , ( "cid", Encode.object [ ( "/", Encode.string link.cid ) ] )
        , ( "mimetype", Encode.string <| Mime.toString <| Maybe.withDefault (Mime.OtherMimeType "") link.mimetype )
        , ( "description", Encode.string link.description )
        ]


contentEncoder : Content -> Encode.Value
contentEncoder =
    Encode.list linkEncoder
