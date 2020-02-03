module Repo exposing (Author, Changes, Commit, Draft, Node, Remote(..), Repo, authorDecoder, authorEncoder, commitDecoder, commitEncoder, defaultRepos, encoder, fetchChildren, ipldNodeEncoder, label, list, menuLink, nodeChanged, nodeDecoder, nodeEncoderForLocalStorage, pathToNodeFolder, repoDecoder, reposDecoder, reposEncoder, store, storeDefaultRepos, template)

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
import Task exposing (Task)
import Time
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import UI.Colors as Colors
import UI.Fonts
import UI.Icons as Icons
import Url exposing (Url)
import Url.Builder
import Url.Parser



-- MODEL


type ContentAdressable a
    = ContentAdressable ( Hash, Remote a )


type alias Repo =
    { name : String
    , description : String
    , tree : Hash
    , location : List Int
    , icon : Maybe Hash
    , head : Maybe Hash
    , draft : Maybe (Tree Node)
    , unsaved : Changes
    }


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


type Remote a
    = NotAsked
    | Loading
    | LoadingSlowly
    | Success a
    | Failed String


type alias Changes =
    Dict (List Int) Node


type alias Draft =
    Maybe (Tree Node)


toChanges : Draft -> Tree Node -> Changes
toChanges draft remote_tree =
    case draft of
        Just tree ->
            Tree.flatten tree
                |> List.map (\node -> ( node.location, node ))
                |> Dict.fromList

        Nothing ->
            Dict.empty



--applyChangeToDAG : NodeAction -> DAG -> DAG
--applyChangeToDAG action zipper =
--    case action of
--        Modified node ->
--            Zipper.findFromRoot node.location zipper
--                |> Zipper.replaceTree
--        Added node ->


type NodeAction
    = Modified Node
    | Added Node
    | Deleted Node
    | Moved { from : Node, to : Node }



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
    [ "krugozor"
    , "stili"
    , "sobytiya"
    , "normy"
    , "teystviya"
    , "torrenty"
    , "osnascheniya"
    , "priverjiya"
    , "sredstva"
    , "sobraniya"
    , "opasnosti"
    , "propuska"
    ]


template : String -> Repo
template key =
    { name = .title <| config key
    , description = .description <| config key
    , tree = "bafyreichkwxwfwt2mbsiesolneyiu7wei3d7loiaukltsmkwavrrobj2ie"
    , location = []
    , icon = Nothing
    , head = Nothing
    , draft = Nothing
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
        , ( "draft", makeNullable treeEncoder repo.draft )
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


encodeChanges : Changes -> Value
encodeChanges changes =
    Dict.toList changes
        |> List.map (\( k, v ) -> { v | location = k })
        |> Encode.list nodeEncoderForLocalStorage


nodeEncoderForLocalStorage : Node -> Encode.Value
nodeEncoderForLocalStorage node =
    Encode.object
        [ ( "cid", Encode.object [ ( "/", Encode.string node.cid ) ] )
        , ( "description", Encode.string node.description )
        , ( "links", Encode.object [ ( "/", Encode.string node.links ) ] )
        , ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "color", Encode.int node.color )
        , ( "location", Encode.list Encode.int node.location )
        ]


ipldNodeEncoder : Node -> Encode.Value
ipldNodeEncoder node =
    Encode.object
        [ ( "cid", Encode.object [ ( "/", Encode.string node.cid ) ] )
        , ( "description", Encode.string node.description )
        , ( "links", Encode.object [ ( "/", Encode.string node.links ) ] )
        , ( "name", Encode.string node.name )
        , ( "size", Encode.int node.size )
        , ( "color", Encode.int node.color )

        --, ( "location", Encode.list Encode.int node.location )
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
        |> optional "draft" decodeDraft Nothing
        |> required "unsaved" lsChangesDecoder


lsChangesDecoder : Decoder Changes
lsChangesDecoder =
    nodeDecoder
        |> Decode.map (\node -> Tuple.pair node.location node)
        |> Decode.list
        |> Decode.map Dict.fromList


repoToNode : Repo -> Node
repoToNode repo =
    { id = 0
    , editing = False
    , name = repo.name
    , cid = ""
    , links = ""
    , size = 0
    , description = repo.description
    , color = 0
    , location = repo.location
    , expanded = False
    }


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


treeEncoder : Tree Node -> Encode.Value
treeEncoder tree =
    Encode.object
        [ ( "label", ipldNodeEncoder <| Tree.label tree )
        , ( "children", Encode.list treeEncoder (Tree.children tree) )
        ]


decodeDraft : Decoder (Maybe (Tree Node))
decodeDraft =
    Decode.nullable decodeTree


decodeTree : Decode.Decoder (Tree Node)
decodeTree =
    Decode.map2 Tree.tree
        (Decode.field "label" nodeDecoder)
        (Decode.field "children" (Decode.lazy (\_ -> Decode.list decodeTree)))



-- HELPERS
-- convert node location [0,1,2] in repo "repo1" to absolute path in IPFS filestore "/suzdal/repos/repo1/links/0/links/1/links/2"


pathToNodeFolder : String -> List Int -> String
pathToNodeFolder repo_key location =
    List.map String.fromInt location
        |> List.intersperse "links"
        |> List.append [ pathToRepo repo_key ]
        |> String.join "/"



-- path to repo; for repo "repo1" it will be - /suzdal/repos/repo1


pathToRepo : String -> String
pathToRepo key =
    String.join "/" [ "suzdal", "repos", key ]
        |> String.append "/"


nodeChanged : List Int -> Changes -> Bool
nodeChanged location changes =
    Dict.member location changes


config : String -> Config msg
config key =
    case key of
        "krugozor" ->
            Config "Описание раздела трактования" "Кругозор" "LimeGreen" Icons.crosshair

        "stili" ->
            Config "Описание раздела изданий" "Стили" "LimeGreen" Icons.smile

        "sobytiya" ->
            Config "Описание раздела событий" "События" "MediumOrchid" Icons.globe

        "normy" ->
            Config "Описание раздела нормативов" "Нормы" "MediumOrchid" Icons.layers

        "teystviya" ->
            Config "Описание раздела затевания" "Тействия" "LightSeaGreen" Icons.activity

        "torrenty" ->
            Config "Описание раздела оргсетей" "Торренты" "LightSeaGreen" Icons.compass

        "osnascheniya" ->
            Config "Описание раздела снабжения" "Оснащения" "OrangeRed" Icons.tool

        "priverjiya" ->
            Config "Описание раздела привержий" "Привержия" "OrangeRed" Icons.heart

        "sredstva" ->
            Config "Описание раздела ресурсов" "Средства" "SteelBlue" Icons.batteryCharging

        "sobraniya" ->
            Config "Описание раздела собраний" "Собрания" "SteelBlue" Icons.users

        "opasnosti" ->
            Config "Описание раздела охраны" "Опасности" "Gold" Icons.shield

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



-- FILES API PART **** DEPRECATED ****


fetchRepo : Url -> String -> Task Http.Error Repo
fetchRepo url repo_key =
    Api.task "GET" (Endpoint.filesRead url <| pathToRepo repo_key ++ "/repo.json") Http.emptyBody repoDecoder


fetchChildren : Url -> Node -> String -> Task Http.Error (List Node)
fetchChildren url node repo_key =
    Decode.list folderDecoder
        |> Api.task "GET" (Endpoint.filesLs url <| pathToNodeFolder repo_key node.location ++ "/links") Http.emptyBody
        |> Task.andThen (Task.sequence << List.map (fetchNode url) << List.filter isLink)


fetchNode : Url -> IpfsObject -> Task Http.Error Node
fetchNode url obj =
    Api.task "GET" (Endpoint.dagGet url <| obj.hash ++ "/node.json") Http.emptyBody nodeDecoder


fetchObject : Url -> Hash -> Task Http.Error IpfsObject
fetchObject url hash =
    Api.task "GET" (Endpoint.objectGet url hash) Http.emptyBody folderDecoder


type alias Object =
    { links : List IpfsObject
    , data : String
    }


type alias IpfsObject =
    { name : String
    , size : Int
    , hash : Hash
    }



-- OBJECT API TYPES FOR DECODE IPFS RESPONSES


objectDecoder : Decode.Decoder Object
objectDecoder =
    Decode.succeed Object
        |> required "Links" (Decode.list folderDecoder)
        |> required "Data" Decode.string


folderDecoder : Decode.Decoder IpfsObject
folderDecoder =
    Decode.succeed IpfsObject
        |> required "Name" Decode.string
        |> required "Size" Decode.int
        |> required "Hash" Decode.string


type alias ObjectStat =
    { hash : String
    , numLinks : Int
    , blockSize : Int
    , linksSize : Int
    , dataSize : Int
    , cumulativeSize : Int
    }


isLink : IpfsObject -> Bool
isLink obj =
    case String.toInt obj.name of
        Just _ ->
            True

        Nothing ->
            False
