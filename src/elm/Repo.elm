module Repo exposing (Author, Changes, Commit, Node, Repo, authorDecoder, authorEncoder, commitDecoder, commitEncoder, defaultRepos, encoder, ipldNodeEncoder, isChanged, label, list, menuLink, nodeDecoder, repoDecoder, reposDecoder, reposEncoder, store, storeDefaultRepos, template)

import Api exposing (Hash)
import Api.Endpoint as Endpoint
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Email exposing (Email)
import Html exposing (Html)
import Html.Attributes
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Time
import UI.Colors as Colors
import UI.Fonts
import UI.Icons as Icons
import Url
import Url.Builder
import Url.Parser



-- MODEL


type ContentAdressable a
    = ContentAdressable ( Hash, Remote a )


type Remote a
    = NotAsked
    | Loading
    | LoadingSlowly
    | Success a
    | Failed


type alias Repo =
    { name : String
    , description : String
    , tree : Hash
    , location : List Int
    , icon : Maybe Hash
    , head : Maybe Hash
    , unsaved : Changes
    }


type alias Changes =
    Dict (List Int) Node


type alias Commit =
    { author : Author
    , date : Time.Posix
    , message : String
    , parent : Maybe Hash
    , tree : Hash
    }


type alias Node =
    { id : Int
    , editing : Bool
    , name : String
    , cid : Hash
    , links : Hash
    , size : Int
    , description : String
    , color : Int
    , location : List Int
    , expanded : Bool
    }


type alias Author =
    { email : String
    , name : String
    }


type alias Config msg =
    { description : String
    , title : String
    , color : String
    , icon : Html msg
    }



-- VIEW


label : String -> Element msg
label key =
    let
        c =
            config key
    in
    link
        [ htmlAttribute <| Html.Attributes.title c.title
        , htmlAttribute <| Html.Attributes.style "color" c.color
        , mouseOver <| [ Background.color <| Colors.darkGrey 0.1 ]
        , padding 20
        ]
        { url = Url.Builder.relative [ "#", "repo", key ] []
        , label =
            row
                [ spacing 25 ]
                [ el [ width <| px 70 ] (html c.icon)
                , column
                    [ spacing 15, width <| px 300 ]
                    [ el UI.Fonts.title (text c.title), text c.description ]
                ]
        }


menuLink : String -> Element msg
menuLink key =
    let
        c =
            config key
    in
    el [ htmlAttribute <| Html.Attributes.style "color" c.color ] <|
        link
            [ htmlAttribute <| Html.Attributes.title c.title
            ]
            { url = Url.Builder.relative [ "#", "repo", key ] []
            , label = el [ width <| px 30 ] <| html c.icon
            }



-- COMMANDS


store : Dict String Repo -> Cmd msg
store repos =
    Api.storeObject ( "repos", reposEncoder repos )


storeDefaultRepos : Cmd msg
storeDefaultRepos =
    store defaultRepos



-- TEMPLATE


defaultRepos : Dict String Repo
defaultRepos =
    List.map (\k -> Tuple.pair k (template k)) list
        |> Dict.fromList


list =
    [ "traktovanie"
    , "izdaniya"
    , "sobytiya"
    , "normativy"
    , "zatevanie"
    , "orgseti"
    , "snabjenie"
    , "priverjiya"
    , "resources"
    , "sobraniya"
    , "ohrana"
    , "propuska"
    ]


template : String -> Repo
template key =
    { name = .title <| config key
    , description = .description <| config key
    , tree = "bafyreif4qskffnc7wy2n3l2pzqkuptea4rvixrenkgsymsnv3qffcgrz5q"
    , location = []
    , icon = Nothing
    , head = Nothing
    , unsaved = Dict.empty
    }



-- ENCODERS


reposEncoder : Dict String Repo -> Encode.Value
reposEncoder repos =
    Encode.dict identity encoder repos


encoder : Repo -> Value
encoder repo =
    Encode.object
        [ ( "name", Encode.string repo.name )
        , ( "description", Encode.string repo.description )
        , ( "tree", ipldLinkEncoder repo.tree )
        , ( "location", Encode.list Encode.int repo.location )
        , ( "icon", makeNullable ipldLinkEncoder repo.icon )
        , ( "head", makeNullable ipldLinkEncoder repo.head )
        , ( "unsaved", encodeChanges repo.unsaved )
        ]


commitEncoder : Commit -> Value
commitEncoder commit =
    Encode.object
        [ ( "author", authorEncoder commit.author )
        , ( "date", Iso8601.encode commit.date )
        , ( "message", Encode.string commit.message )
        , ( "parent", makeNullable ipldLinkEncoder commit.parent )
        , ( "tree", ipldLinkEncoder commit.tree )
        ]


encodeChanges : Changes -> Encode.Value
encodeChanges changes =
    Dict.toList changes
        |> Encode.list
            (\( location, node ) ->
                Encode.object
                    [ ( "location", Encode.list Encode.int location )
                    , ( "node", ipldNodeEncoder node )
                    ]
            )


authorEncoder : Author -> Value
authorEncoder author =
    Encode.object
        [ ( "email", Encode.string author.email )
        , ( "name", Encode.string author.name )
        ]


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "email" Decode.string
        |> required "name" Decode.string


ipldLinkEncoder : Hash -> Value
ipldLinkEncoder hash =
    Encode.object [ ( "/", Encode.string hash ) ]


ipldNodeEncoder : Node -> Encode.Value
ipldNodeEncoder node =
    Encode.object
        [ ( "cid", Encode.object [ ( "/", Encode.string node.cid ) ] )
        , ( "description", Encode.string node.description )
        , ( "links", Encode.object [ ( "/", Encode.string node.links ) ] )
        , ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "color", Encode.int node.color )
        , ( "location", Encode.list Encode.int node.location )
        ]


makeNullable : (a -> Encode.Value) -> Maybe a -> Encode.Value
makeNullable f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Encode.null



-- DECODERS


reposDecoder : Decoder (Dict String Repo)
reposDecoder =
    Decode.dict repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    Decode.succeed Repo
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "tree" (Decode.at [ "/" ] Decode.string)
        |> optional "location" (Decode.list Decode.int) []
        |> required "icon" ipldLinkDecoder
        |> required "head" ipldLinkDecoder
        |> optional "unsaved" decodeChanges Dict.empty


commitDecoder : Decoder Commit
commitDecoder =
    Decode.succeed Commit
        |> required "author" authorDecoder
        |> required "date" Iso8601.decoder
        |> required "message" Decode.string
        |> required "parent" ipldLinkDecoder
        |> required "tree" (Decode.at [ "/" ] Decode.string)


ipldLinkDecoder : Decoder (Maybe String)
ipldLinkDecoder =
    Decode.at [ "/" ] Decode.string
        |> Decode.nullable


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> hardcoded 0
        |> hardcoded False
        |> optional "name" Decode.string "0"
        |> required "cid" (Decode.field "/" Decode.string)
        |> required "links" (Decode.field "/" Decode.string)
        |> optional "size" Decode.int 0
        |> optional "description" Decode.string ""
        |> optional "color" Decode.int 0
        |> optional "location" (Decode.list Decode.int) []
        |> hardcoded False


type alias JsonChange =
    { location : List Int, node : Node }


jsonChangeDecode : Decoder JsonChange
jsonChangeDecode =
    Decode.succeed JsonChange
        |> required "location" (Decode.list Decode.int)
        |> required "node" nodeDecoder


decodeChanges : Decoder Changes
decodeChanges =
    Decode.list jsonChangeDecode
        |> Decode.andThen (Decode.succeed << jsonChangesToDict)


jsonChangesToDict : List JsonChange -> Changes
jsonChangesToDict =
    Dict.fromList << List.map (\change -> ( change.location, change.node ))



-- COMMANDS
--fromFile : (Result Http.Error (List Repo) -> msg) -> Cmd msg
--fromFile trigger =
--    Api.get Endpoint.getInitRepos trigger (Decode.list repoDecoder)
-- HELPERS


isChanged : List Int -> Changes -> Bool
isChanged location changes =
    Dict.member location changes



--


config : String -> Config msg
config key =
    case key of
        "traktovanie" ->
            Config "Описание раздела трактования" "Трактование" "LimeGreen" Icons.crosshair

        "izdaniya" ->
            Config "Описание раздела изданий" "Издания" "LimeGreen" Icons.printer

        "sobytiya" ->
            Config "Описание раздела событий" "События" "MediumOrchid" Icons.calendar

        "normativy" ->
            Config "Описание раздела нормативов" "Нормативы" "MediumOrchid" Icons.globe

        "zatevanie" ->
            Config "Описание раздела затевания" "Затевание" "LightSeaGreen" Icons.sun

        "orgseti" ->
            Config "Описание раздела оргсетей" "Оргсети" "LightSeaGreen" Icons.mail

        "snabjenie" ->
            Config "Описание раздела снабжения" "Снабжение" "OrangeRed" Icons.truck

        "priverjiya" ->
            Config "Описание раздела привержий" "Привержия" "OrangeRed" Icons.heart

        "resources" ->
            Config "Описание раздела ресурсов" "Ресурсы" "SteelBlue" Icons.shoppingCart

        "sobraniya" ->
            Config "Описание раздела собраний" "Собрания" "SteelBlue" Icons.users

        "ohrana" ->
            Config "Описание раздела охраны" "Охрана" "Gold" Icons.lock

        "propuska" ->
            Config "Описание раздела пропусков" "Пропуска" "Gold" Icons.key

        s ->
            Config s "Unnamed repo" "White" Icons.xSquare


type RepoType
    = Interprete
    | Publish
    | Event
    | Standard
    | Startup
    | Logistics
    | Supply
    | Ideology
    | Resourse
    | Meeting
    | Security
    | Access
