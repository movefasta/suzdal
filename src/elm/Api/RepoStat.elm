module Api.RepoStat exposing
    ( RepoStat
    , decoder
    , repoStatToString
    )

import Dict exposing (Dict, map, toList)
import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import List exposing (map)


type alias RepoStat =
    { repoSize : Maybe Int
    , storageMax : Maybe Int
    , numObjects : Maybe Int
    , repoPath : Maybe String
    , version : Maybe String
    }



-- decoders and encoders


repoStatToString : RepoStat -> String
repoStatToString r =
    Jenc.encode 0 (encodeRepoStat r)


decoder : Jdec.Decoder RepoStat
decoder =
    Jdec.succeed RepoStat
        |> Jpipe.optional "RepoSize" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "StorageMax" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "NumObjects" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "RepoPath" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Version" (Jdec.nullable Jdec.string) Nothing


encodeRepoStat : RepoStat -> Jenc.Value
encodeRepoStat x =
    Jenc.object
        [ ( "RepoSize", makeNullableEncoder Jenc.int x.repoSize )
        , ( "StorageMax", makeNullableEncoder Jenc.int x.storageMax )
        , ( "NumObjects", makeNullableEncoder Jenc.int x.numObjects )
        , ( "RepoPath", makeNullableEncoder Jenc.string x.repoPath )
        , ( "Version", makeNullableEncoder Jenc.string x.version )
        ]



--- encoder helpers


makeNullableEncoder : (a -> Jenc.Value) -> Maybe a -> Jenc.Value
makeNullableEncoder f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Jenc.null
