module Api.SwarmPeers exposing
    ( SwarmPeers
    , amount
    , decoder
    , encoder
    , inSwarm
    )

import Dict exposing (Dict, map, toList)
import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import List exposing (map)


type alias SwarmPeers =
    { peers : Maybe (List Peer)
    }


type alias Peer =
    { addr : Maybe String
    , peer : String
    , latency : Maybe String
    , muxer : Maybe String
    , direction : Maybe Int
    , streams : Maybe ()
    }


amount : SwarmPeers -> Int
amount response =
    case response.peers of
        Just list ->
            List.length list

        Nothing ->
            0


inSwarm : String -> SwarmPeers -> Bool
inSwarm hash swarm =
    case swarm.peers of
        Just list ->
            List.map .peer list
                |> List.member hash

        Nothing ->
            False



-- decoders and encoders


encoder : SwarmPeers -> String
encoder r =
    Jenc.encode 0 (encodeSwarmPeers r)


decoder : Jdec.Decoder SwarmPeers
decoder =
    Jdec.succeed SwarmPeers
        |> Jpipe.optional "Peers" (Jdec.nullable (Jdec.list peer)) Nothing


encodeSwarmPeers : SwarmPeers -> Jenc.Value
encodeSwarmPeers x =
    Jenc.object
        [ ( "Peers", makeNullableEncoder (Jenc.list encodePeer) x.peers )
        ]


peer : Jdec.Decoder Peer
peer =
    Jdec.succeed Peer
        |> Jpipe.optional "Addr" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.required "Peer" Jdec.string
        |> Jpipe.optional "Latency" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Muxer" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Direction" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "Streams" (Jdec.nullable (Jdec.null ())) Nothing


encodePeer : Peer -> Jenc.Value
encodePeer x =
    Jenc.object
        [ ( "Addr", makeNullableEncoder Jenc.string x.addr )
        , ( "Peer", Jenc.string x.peer )
        , ( "Latency", makeNullableEncoder Jenc.string x.latency )
        , ( "Muxer", makeNullableEncoder Jenc.string x.muxer )
        , ( "Direction", makeNullableEncoder Jenc.int x.direction )
        , ( "Streams", makeNullableEncoder (always Jenc.null) x.streams )
        ]



--- encoder helpers


makeDictEncoder : (a -> Jenc.Value) -> Dict String a -> Jenc.Value
makeDictEncoder f dict =
    Jenc.object (toList (Dict.map (\k -> f) dict))


makeNullableEncoder : (a -> Jenc.Value) -> Maybe a -> Jenc.Value
makeNullableEncoder f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Jenc.null
