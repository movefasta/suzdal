module Api.NodeConfig exposing
    ( API
    , Addresses
    , Child
    , ConnMgr
    , Datastore
    , Discovery
    , Experimental
    , Gateway
    , HTTPHeaders
    , Identity
    , IpfsNodeConfig
    , Ipns
    , Mdns
    , Mount
    , Mounts
    , Pubsub
    , Reprovider
    , Routing
    , Spec
    , Swarm
    , decoder
    , encoder
    )

import Dict exposing (Dict, map, toList)
import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import List exposing (map)


type alias IpfsNodeConfig =
    { api : Maybe API
    , addresses : Maybe Addresses
    , bootstrap : Maybe (List String)
    , datastore : Maybe Datastore
    , discovery : Maybe Discovery
    , experimental : Maybe Experimental
    , gateway : Maybe Gateway
    , ipfsNodeConfigIdentity : Maybe Identity
    , ipns : Maybe Ipns
    , mounts : Maybe Mounts
    , pubsub : Maybe Pubsub
    , reprovider : Maybe Reprovider
    , routing : Maybe Routing
    , swarm : Maybe Swarm
    }


type alias Addresses =
    { api : Maybe String
    , announce : Maybe (List Jdec.Value)
    , gateway : Maybe String
    , noAnnounce : Maybe (List Jdec.Value)
    , swarm : Maybe (List String)
    }


type alias API =
    { httpHeaders : Maybe HTTPHeaders
    }


type alias HTTPHeaders =
    { accessControlAllowHeaders : Maybe (List String)
    , accessControlAllowMethods : Maybe (List String)
    , accessControlAllowOrigin : Maybe (List String)
    }


type alias Datastore =
    { bloomFilterSize : Maybe Int
    , gcPeriod : Maybe String
    , hashOnRead : Maybe Bool
    , spec : Maybe Spec
    , storageGCWatermark : Maybe Int
    , storageMax : Maybe String
    }


type alias Spec =
    { mounts : Maybe (List Mount)
    , specType : Maybe String
    }


type alias Mount =
    { child : Maybe Child
    , mountpoint : Maybe String
    , prefix : Maybe String
    , mountType : Maybe String
    }


type alias Child =
    { path : Maybe String
    , shardFunc : Maybe String
    , sync : Maybe Bool
    , childType : Maybe String
    , compression : Maybe String
    }


type alias Discovery =
    { mdns : Maybe Mdns
    }


type alias Mdns =
    { enabled : Maybe Bool
    , interval : Maybe Int
    }


type alias Experimental =
    { filestoreEnabled : Maybe Bool
    , libp2PStreamMounting : Maybe Bool
    , p2PHTTPProxy : Maybe Bool
    , quic : Maybe Bool
    , shardingEnabled : Maybe Bool
    , urlstoreEnabled : Maybe Bool
    }


type alias Gateway =
    { apiCommands : Maybe (List String)
    , httpHeaders : Maybe HTTPHeaders
    , noFetch : Maybe Bool
    , pathPrefixes : Maybe (List Jdec.Value)
    , rootRedirect : Maybe String
    , writable : Maybe Bool
    }


type alias Identity =
    { peerID : Maybe String
    }


type alias Ipns =
    { recordLifetime : Maybe String
    , republishPeriod : Maybe String
    , resolveCacheSize : Maybe Int
    }


type alias Mounts =
    { fuseAllowOther : Maybe Bool
    , ipfs : Maybe String
    , ipns : Maybe String
    }


type alias Pubsub =
    { disableSigning : Maybe Bool
    , router : Maybe String
    , strictSignatureVerification : Maybe Bool
    }


type alias Reprovider =
    { interval : Maybe String
    , strategy : Maybe String
    }


type alias Routing =
    { routingType : Maybe String
    }


type alias Swarm =
    { addrFilters : Maybe (List String)
    , connMgr : Maybe ConnMgr
    , disableBandwidthMetrics : Maybe Bool
    , disableNatPortMap : Maybe Bool
    , disableRelay : Maybe Bool
    , enableAutoNATService : Maybe Bool
    , enableAutoRelay : Maybe Bool
    , enableRelayHop : Maybe Bool
    }


type alias ConnMgr =
    { gracePeriod : Maybe String
    , highWater : Maybe Int
    , lowWater : Maybe Int
    , connMgrType : Maybe String
    }



-- decoders and encoders


encoder : IpfsNodeConfig -> String
encoder r =
    Jenc.encode 4 (encodeIpfsNodeConfig r)


decoder : Jdec.Decoder IpfsNodeConfig
decoder =
    Jdec.succeed IpfsNodeConfig
        |> Jpipe.optional "API" (Jdec.nullable api) Nothing
        |> Jpipe.optional "Addresses" (Jdec.nullable addresses) Nothing
        |> Jpipe.optional "Bootstrap" (Jdec.nullable (Jdec.list Jdec.string)) Nothing
        |> Jpipe.optional "Datastore" (Jdec.nullable datastore) Nothing
        |> Jpipe.optional "Discovery" (Jdec.nullable discovery) Nothing
        |> Jpipe.optional "Experimental" (Jdec.nullable experimental) Nothing
        |> Jpipe.optional "Gateway" (Jdec.nullable gateway) Nothing
        |> Jpipe.optional "Identity" (Jdec.nullable purpleIdentity) Nothing
        |> Jpipe.optional "Ipns" (Jdec.nullable ipns) Nothing
        |> Jpipe.optional "Mounts" (Jdec.nullable mounts) Nothing
        |> Jpipe.optional "Pubsub" (Jdec.nullable pubsub) Nothing
        |> Jpipe.optional "Reprovider" (Jdec.nullable reprovider) Nothing
        |> Jpipe.optional "Routing" (Jdec.nullable routing) Nothing
        |> Jpipe.optional "Swarm" (Jdec.nullable swarm) Nothing


encodeIpfsNodeConfig : IpfsNodeConfig -> Jenc.Value
encodeIpfsNodeConfig x =
    Jenc.object
        [ ( "API", makeNullableEncoder encodeAPI x.api )
        , ( "Addresses", makeNullableEncoder encodeAddresses x.addresses )
        , ( "Bootstrap", makeNullableEncoder (makeListEncoder Jenc.string) x.bootstrap )
        , ( "Datastore", makeNullableEncoder encodeDatastore x.datastore )
        , ( "Discovery", makeNullableEncoder encodeDiscovery x.discovery )
        , ( "Experimental", makeNullableEncoder encodeExperimental x.experimental )
        , ( "Gateway", makeNullableEncoder encodeGateway x.gateway )
        , ( "Identity", makeNullableEncoder encodeIdentity x.ipfsNodeConfigIdentity )
        , ( "Ipns", makeNullableEncoder encodeIpns x.ipns )
        , ( "Mounts", makeNullableEncoder encodeMounts x.mounts )
        , ( "Pubsub", makeNullableEncoder encodePubsub x.pubsub )
        , ( "Reprovider", makeNullableEncoder encodeReprovider x.reprovider )
        , ( "Routing", makeNullableEncoder encodeRouting x.routing )
        , ( "Swarm", makeNullableEncoder encodeSwarm x.swarm )
        ]


addresses : Jdec.Decoder Addresses
addresses =
    Jdec.succeed Addresses
        |> Jpipe.optional "API" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Announce" (Jdec.nullable (Jdec.list Jdec.value)) Nothing
        |> Jpipe.optional "Gateway" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "NoAnnounce" (Jdec.nullable (Jdec.list Jdec.value)) Nothing
        |> Jpipe.optional "Swarm" (Jdec.nullable (Jdec.list Jdec.string)) Nothing


encodeAddresses : Addresses -> Jenc.Value
encodeAddresses x =
    Jenc.object
        [ ( "API", makeNullableEncoder Jenc.string x.api )
        , ( "Announce", makeNullableEncoder (makeListEncoder identity) x.announce )
        , ( "Gateway", makeNullableEncoder Jenc.string x.gateway )
        , ( "NoAnnounce", makeNullableEncoder (makeListEncoder identity) x.noAnnounce )
        , ( "Swarm", makeNullableEncoder (makeListEncoder Jenc.string) x.swarm )
        ]


api : Jdec.Decoder API
api =
    Jdec.succeed API
        |> Jpipe.optional "HTTPHeaders" (Jdec.nullable httpHeaders) Nothing


encodeAPI : API -> Jenc.Value
encodeAPI x =
    Jenc.object
        [ ( "HTTPHeaders", makeNullableEncoder encodeHTTPHeaders x.httpHeaders )
        ]


httpHeaders : Jdec.Decoder HTTPHeaders
httpHeaders =
    Jdec.succeed HTTPHeaders
        |> Jpipe.optional "Access-Control-Allow-Headers" (Jdec.nullable (Jdec.list Jdec.string)) Nothing
        |> Jpipe.optional "Access-Control-Allow-Methods" (Jdec.nullable (Jdec.list Jdec.string)) Nothing
        |> Jpipe.optional "Access-Control-Allow-Origin" (Jdec.nullable (Jdec.list Jdec.string)) Nothing


encodeHTTPHeaders : HTTPHeaders -> Jenc.Value
encodeHTTPHeaders x =
    Jenc.object
        [ ( "Access-Control-Allow-Headers", makeNullableEncoder (makeListEncoder Jenc.string) x.accessControlAllowHeaders )
        , ( "Access-Control-Allow-Methods", makeNullableEncoder (makeListEncoder Jenc.string) x.accessControlAllowMethods )
        , ( "Access-Control-Allow-Origin", makeNullableEncoder (makeListEncoder Jenc.string) x.accessControlAllowOrigin )
        ]


datastore : Jdec.Decoder Datastore
datastore =
    Jdec.succeed Datastore
        |> Jpipe.optional "BloomFilterSize" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "GCPeriod" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "HashOnRead" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "Spec" (Jdec.nullable spec) Nothing
        |> Jpipe.optional "StorageGCWatermark" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "StorageMax" (Jdec.nullable Jdec.string) Nothing


encodeDatastore : Datastore -> Jenc.Value
encodeDatastore x =
    Jenc.object
        [ ( "BloomFilterSize", makeNullableEncoder Jenc.int x.bloomFilterSize )
        , ( "GCPeriod", makeNullableEncoder Jenc.string x.gcPeriod )
        , ( "HashOnRead", makeNullableEncoder Jenc.bool x.hashOnRead )
        , ( "Spec", makeNullableEncoder encodeSpec x.spec )
        , ( "StorageGCWatermark", makeNullableEncoder Jenc.int x.storageGCWatermark )
        , ( "StorageMax", makeNullableEncoder Jenc.string x.storageMax )
        ]


spec : Jdec.Decoder Spec
spec =
    Jdec.succeed Spec
        |> Jpipe.optional "mounts" (Jdec.nullable (Jdec.list mount)) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing


encodeSpec : Spec -> Jenc.Value
encodeSpec x =
    Jenc.object
        [ ( "mounts", makeNullableEncoder (makeListEncoder encodeMount) x.mounts )
        , ( "type", makeNullableEncoder Jenc.string x.specType )
        ]


mount : Jdec.Decoder Mount
mount =
    Jdec.succeed Mount
        |> Jpipe.optional "child" (Jdec.nullable child) Nothing
        |> Jpipe.optional "mountpoint" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "prefix" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing


encodeMount : Mount -> Jenc.Value
encodeMount x =
    Jenc.object
        [ ( "child", makeNullableEncoder encodeChild x.child )
        , ( "mountpoint", makeNullableEncoder Jenc.string x.mountpoint )
        , ( "prefix", makeNullableEncoder Jenc.string x.prefix )
        , ( "type", makeNullableEncoder Jenc.string x.mountType )
        ]


child : Jdec.Decoder Child
child =
    Jdec.succeed Child
        |> Jpipe.optional "path" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "shardFunc" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "sync" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "type" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "compression" (Jdec.nullable Jdec.string) Nothing


encodeChild : Child -> Jenc.Value
encodeChild x =
    Jenc.object
        [ ( "path", makeNullableEncoder Jenc.string x.path )
        , ( "shardFunc", makeNullableEncoder Jenc.string x.shardFunc )
        , ( "sync", makeNullableEncoder Jenc.bool x.sync )
        , ( "type", makeNullableEncoder Jenc.string x.childType )
        , ( "compression", makeNullableEncoder Jenc.string x.compression )
        ]


discovery : Jdec.Decoder Discovery
discovery =
    Jdec.succeed Discovery
        |> Jpipe.optional "MDNS" (Jdec.nullable mdns) Nothing


encodeDiscovery : Discovery -> Jenc.Value
encodeDiscovery x =
    Jenc.object
        [ ( "MDNS", makeNullableEncoder encodeMdns x.mdns )
        ]


mdns : Jdec.Decoder Mdns
mdns =
    Jdec.succeed Mdns
        |> Jpipe.optional "Enabled" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "Interval" (Jdec.nullable Jdec.int) Nothing


encodeMdns : Mdns -> Jenc.Value
encodeMdns x =
    Jenc.object
        [ ( "Enabled", makeNullableEncoder Jenc.bool x.enabled )
        , ( "Interval", makeNullableEncoder Jenc.int x.interval )
        ]


experimental : Jdec.Decoder Experimental
experimental =
    Jdec.succeed Experimental
        |> Jpipe.optional "FilestoreEnabled" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "Libp2pStreamMounting" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "P2pHttpProxy" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "QUIC" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "ShardingEnabled" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "UrlstoreEnabled" (Jdec.nullable Jdec.bool) Nothing


encodeExperimental : Experimental -> Jenc.Value
encodeExperimental x =
    Jenc.object
        [ ( "FilestoreEnabled", makeNullableEncoder Jenc.bool x.filestoreEnabled )
        , ( "Libp2pStreamMounting", makeNullableEncoder Jenc.bool x.libp2PStreamMounting )
        , ( "P2pHttpProxy", makeNullableEncoder Jenc.bool x.p2PHTTPProxy )
        , ( "QUIC", makeNullableEncoder Jenc.bool x.quic )
        , ( "ShardingEnabled", makeNullableEncoder Jenc.bool x.shardingEnabled )
        , ( "UrlstoreEnabled", makeNullableEncoder Jenc.bool x.urlstoreEnabled )
        ]


gateway : Jdec.Decoder Gateway
gateway =
    Jdec.succeed Gateway
        |> Jpipe.optional "APICommands" (Jdec.nullable (Jdec.list Jdec.string)) Nothing
        |> Jpipe.optional "HTTPHeaders" (Jdec.nullable httpHeaders) Nothing
        |> Jpipe.optional "NoFetch" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "PathPrefixes" (Jdec.nullable (Jdec.list Jdec.value)) Nothing
        |> Jpipe.optional "RootRedirect" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Writable" (Jdec.nullable Jdec.bool) Nothing


encodeGateway : Gateway -> Jenc.Value
encodeGateway x =
    Jenc.object
        [ ( "APICommands", makeNullableEncoder (always Jenc.null) x.apiCommands )
        , ( "HTTPHeaders", makeNullableEncoder encodeHTTPHeaders x.httpHeaders )
        , ( "NoFetch", makeNullableEncoder Jenc.bool x.noFetch )
        , ( "PathPrefixes", makeNullableEncoder (makeListEncoder identity) x.pathPrefixes )
        , ( "RootRedirect", makeNullableEncoder Jenc.string x.rootRedirect )
        , ( "Writable", makeNullableEncoder Jenc.bool x.writable )
        ]


purpleIdentity : Jdec.Decoder Identity
purpleIdentity =
    Jdec.succeed Identity
        |> Jpipe.optional "PeerID" (Jdec.nullable Jdec.string) Nothing


encodeIdentity : Identity -> Jenc.Value
encodeIdentity x =
    Jenc.object
        [ ( "PeerID", makeNullableEncoder Jenc.string x.peerID )
        ]


ipns : Jdec.Decoder Ipns
ipns =
    Jdec.succeed Ipns
        |> Jpipe.optional "RecordLifetime" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "RepublishPeriod" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "ResolveCacheSize" (Jdec.nullable Jdec.int) Nothing


encodeIpns : Ipns -> Jenc.Value
encodeIpns x =
    Jenc.object
        [ ( "RecordLifetime", makeNullableEncoder Jenc.string x.recordLifetime )
        , ( "RepublishPeriod", makeNullableEncoder Jenc.string x.republishPeriod )
        , ( "ResolveCacheSize", makeNullableEncoder Jenc.int x.resolveCacheSize )
        ]


mounts : Jdec.Decoder Mounts
mounts =
    Jdec.succeed Mounts
        |> Jpipe.optional "FuseAllowOther" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "IPFS" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "IPNS" (Jdec.nullable Jdec.string) Nothing


encodeMounts : Mounts -> Jenc.Value
encodeMounts x =
    Jenc.object
        [ ( "FuseAllowOther", makeNullableEncoder Jenc.bool x.fuseAllowOther )
        , ( "IPFS", makeNullableEncoder Jenc.string x.ipfs )
        , ( "IPNS", makeNullableEncoder Jenc.string x.ipns )
        ]


pubsub : Jdec.Decoder Pubsub
pubsub =
    Jdec.succeed Pubsub
        |> Jpipe.optional "DisableSigning" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "Router" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "StrictSignatureVerification" (Jdec.nullable Jdec.bool) Nothing


encodePubsub : Pubsub -> Jenc.Value
encodePubsub x =
    Jenc.object
        [ ( "DisableSigning", makeNullableEncoder Jenc.bool x.disableSigning )
        , ( "Router", makeNullableEncoder Jenc.string x.router )
        , ( "StrictSignatureVerification", makeNullableEncoder Jenc.bool x.strictSignatureVerification )
        ]


reprovider : Jdec.Decoder Reprovider
reprovider =
    Jdec.succeed Reprovider
        |> Jpipe.optional "Interval" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "Strategy" (Jdec.nullable Jdec.string) Nothing


encodeReprovider : Reprovider -> Jenc.Value
encodeReprovider x =
    Jenc.object
        [ ( "Interval", makeNullableEncoder Jenc.string x.interval )
        , ( "Strategy", makeNullableEncoder Jenc.string x.strategy )
        ]


routing : Jdec.Decoder Routing
routing =
    Jdec.succeed Routing
        |> Jpipe.optional "Type" (Jdec.nullable Jdec.string) Nothing


encodeRouting : Routing -> Jenc.Value
encodeRouting x =
    Jenc.object
        [ ( "Type", makeNullableEncoder Jenc.string x.routingType )
        ]


swarm : Jdec.Decoder Swarm
swarm =
    Jdec.succeed Swarm
        |> Jpipe.optional "AddrFilters" (Jdec.nullable (Jdec.list Jdec.string)) Nothing
        |> Jpipe.optional "ConnMgr" (Jdec.nullable connMgr) Nothing
        |> Jpipe.optional "DisableBandwidthMetrics" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "DisableNatPortMap" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "DisableRelay" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "EnableAutoNATService" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "EnableAutoRelay" (Jdec.nullable Jdec.bool) Nothing
        |> Jpipe.optional "EnableRelayHop" (Jdec.nullable Jdec.bool) Nothing


encodeSwarm : Swarm -> Jenc.Value
encodeSwarm x =
    Jenc.object
        [ ( "AddrFilters", makeNullableEncoder (always Jenc.null) x.addrFilters )
        , ( "ConnMgr", makeNullableEncoder encodeConnMgr x.connMgr )
        , ( "DisableBandwidthMetrics", makeNullableEncoder Jenc.bool x.disableBandwidthMetrics )
        , ( "DisableNatPortMap", makeNullableEncoder Jenc.bool x.disableNatPortMap )
        , ( "DisableRelay", makeNullableEncoder Jenc.bool x.disableRelay )
        , ( "EnableAutoNATService", makeNullableEncoder Jenc.bool x.enableAutoNATService )
        , ( "EnableAutoRelay", makeNullableEncoder Jenc.bool x.enableAutoRelay )
        , ( "EnableRelayHop", makeNullableEncoder Jenc.bool x.enableRelayHop )
        ]


connMgr : Jdec.Decoder ConnMgr
connMgr =
    Jdec.succeed ConnMgr
        |> Jpipe.optional "GracePeriod" (Jdec.nullable Jdec.string) Nothing
        |> Jpipe.optional "HighWater" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "LowWater" (Jdec.nullable Jdec.int) Nothing
        |> Jpipe.optional "Type" (Jdec.nullable Jdec.string) Nothing


encodeConnMgr : ConnMgr -> Jenc.Value
encodeConnMgr x =
    Jenc.object
        [ ( "GracePeriod", makeNullableEncoder Jenc.string x.gracePeriod )
        , ( "HighWater", makeNullableEncoder Jenc.int x.highWater )
        , ( "LowWater", makeNullableEncoder Jenc.int x.lowWater )
        , ( "Type", makeNullableEncoder Jenc.string x.connMgrType )
        ]



--- encoder helpers


makeListEncoder : (a -> Jenc.Value) -> List a -> Jenc.Value
makeListEncoder f arr =
    Jenc.list f arr


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
