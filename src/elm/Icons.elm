module Icons exposing
    ( alertTriangle
    , arrowDown
    , arrowLeft
    , arrowRight
    , arrowUp
    , bold
    , bookOpen
    , bookmark
    , box
    , check
    , checkSquare
    , chevronDown
    , chevronLeft
    , chevronRight
    , chevronUp
    , clipboard
    , clock
    , copy
    , database
    , delete
    , download
    , edit
    , eye
    , eyeOff
    , filePlus
    , fileText
    , gitBranch
    , gitCommit
    , gitMerge
    , gitPullRequest
    , github
    , grid
    , hash
    , helpCircle
    , hexagon
    , home
    , italic
    , link
    , loader
    , plusCircle
    , save
    , search
    , send
    , server
    , settings
    , share2
    , square
    , tag
    , toggleLeft
    , trash2
    , upload
    , user
    , x
    )

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "30"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "1"
        , viewBox "0 0 24 24"
        , width "30"
        ]


x : Html msg
x =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


user : Html msg
user =
    svgFeatherIcon "user"
        [ Svg.path [ d "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "12", cy "7", r "4" ] []
        ]


server : Html msg
server =
    svgFeatherIcon "server"
        [ Svg.rect [ Svg.Attributes.x "2", y "2", width "20", height "8", rx "2", ry "2" ] []
        , Svg.rect [ Svg.Attributes.x "2", y "14", width "20", height "8", rx "2", ry "2" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "6", y2 "6" ] []
        , Svg.line [ x1 "6", y1 "18", x2 "6", y2 "18" ] []
        ]


bookOpen : Html msg
bookOpen =
    svgFeatherIcon "book-open"
        [ Svg.path [ d "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z" ] []
        , Svg.path [ d "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z" ] []
        ]


eyeOff : Html msg
eyeOff =
    svgFeatherIcon "eye-off"
        [ Svg.path [ d "M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


alertTriangle : Html msg
alertTriangle =
    svgFeatherIcon "alert-triangle"
        [ Svg.path [ d "M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z" ] []
        , Svg.line [ x1 "12", y1 "9", x2 "12", y2 "13" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12", y2 "17" ] []
        ]


arrowDown : Html msg
arrowDown =
    svgFeatherIcon "arrow-down"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.polyline [ points "19 12 12 19 5 12" ] []
        ]


arrowLeft : Html msg
arrowLeft =
    svgFeatherIcon "arrow-left"
        [ Svg.line [ x1 "19", y1 "12", x2 "5", y2 "12" ] []
        , Svg.polyline [ points "12 19 5 12 12 5" ] []
        ]


arrowRight : Html msg
arrowRight =
    svgFeatherIcon "arrow-right"
        [ Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        , Svg.polyline [ points "12 5 19 12 12 19" ] []
        ]


arrowUp : Html msg
arrowUp =
    svgFeatherIcon "arrow-up"
        [ Svg.line [ x1 "12", y1 "19", x2 "12", y2 "5" ] []
        , Svg.polyline [ points "5 12 12 5 19 12" ] []
        ]


bold : Html msg
bold =
    svgFeatherIcon "bold"
        [ Svg.path [ d "M6 4h8a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z" ] []
        , Svg.path [ d "M6 12h9a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z" ] []
        ]


bookmark : Html msg
bookmark =
    svgFeatherIcon "bookmark"
        [ Svg.path [ d "M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z" ] []
        ]


box : Html msg
box =
    svgFeatherIcon "box"
        [ Svg.path [ d "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z" ] []
        , Svg.polyline [ points "3.27 6.96 12 12.01 20.73 6.96" ] []
        , Svg.line [ x1 "12", y1 "22.08", x2 "12", y2 "12" ] []
        ]


check : Html msg
check =
    svgFeatherIcon "check"
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


square : Html msg
square =
    svgFeatherIcon "square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        ]


checkSquare : Html msg
checkSquare =
    svgFeatherIcon "check-square"
        [ Svg.polyline [ points "9 11 12 14 22 4" ] []
        , Svg.path [ d "M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11" ] []
        ]


chevronDown : Html msg
chevronDown =
    svgFeatherIcon "chevron-down"
        [ Svg.polyline [ points "6 9 12 15 18 9" ] []
        ]


chevronLeft : Html msg
chevronLeft =
    svgFeatherIcon "chevron-left"
        [ Svg.polyline [ points "15 18 9 12 15 6" ] []
        ]


chevronRight : Html msg
chevronRight =
    svgFeatherIcon "chevron-right"
        [ Svg.polyline [ points "9 18 15 12 9 6" ] []
        ]


chevronUp : Html msg
chevronUp =
    svgFeatherIcon "chevron-up"
        [ Svg.polyline [ points "18 15 12 9 6 15" ] []
        ]


clipboard : Html msg
clipboard =
    svgFeatherIcon "clipboard"
        [ Svg.path [ d "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2" ] []
        , Svg.rect [ Svg.Attributes.x "8", y "2", width "8", height "4", rx "1", ry "1" ] []
        ]


clock : Html msg
clock =
    svgFeatherIcon "clock"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 6 12 12 16 14" ] []
        ]


copy : Html msg
copy =
    svgFeatherIcon "copy"
        [ Svg.rect [ Svg.Attributes.x "9", y "9", width "13", height "13", rx "2", ry "2" ] []
        , Svg.path [ d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" ] []
        ]


database : Html msg
database =
    svgFeatherIcon "database"
        [ Svg.ellipse [ cx "12", cy "5", rx "9", ry "3" ] []
        , Svg.path [ d "M21 12c0 1.66-4 3-9 3s-9-1.34-9-3" ] []
        , Svg.path [ d "M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5" ] []
        ]


delete : Html msg
delete =
    svgFeatherIcon "delete"
        [ Svg.path [ d "M21 4H8l-7 8 7 8h13a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2z" ] []
        , Svg.line [ x1 "18", y1 "9", x2 "12", y2 "15" ] []
        , Svg.line [ x1 "12", y1 "9", x2 "18", y2 "15" ] []
        ]


download : Html msg
download =
    svgFeatherIcon "download"
        [ Svg.path [ d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" ] []
        , Svg.polyline [ points "7 10 12 15 17 10" ] []
        , Svg.line [ x1 "12", y1 "15", x2 "12", y2 "3" ] []
        ]


edit : Html msg
edit =
    svgFeatherIcon "edit"
        [ Svg.path [ d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7" ] []
        , Svg.path [ d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z" ] []
        ]


eye : Html msg
eye =
    svgFeatherIcon "eye"
        [ Svg.path [ d "M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" ] []
        , Svg.circle [ cx "12", cy "12", r "3" ] []
        ]


filePlus : Html msg
filePlus =
    svgFeatherIcon "file-plus"
        [ Svg.path [ d "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" ] []
        , Svg.polyline [ points "14 2 14 8 20 8" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "9", y1 "15", x2 "15", y2 "15" ] []
        ]


fileText : Html msg
fileText =
    svgFeatherIcon "file-text"
        [ Svg.path [ d "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" ] []
        , Svg.polyline [ points "14 2 14 8 20 8" ] []
        , Svg.line [ x1 "16", y1 "13", x2 "8", y2 "13" ] []
        , Svg.line [ x1 "16", y1 "17", x2 "8", y2 "17" ] []
        , Svg.polyline [ points "10 9 9 9 8 9" ] []
        ]


gitBranch : Html msg
gitBranch =
    svgFeatherIcon "git-branch"
        [ Svg.line [ x1 "6", y1 "3", x2 "6", y2 "15" ] []
        , Svg.circle [ cx "18", cy "6", r "3" ] []
        , Svg.circle [ cx "6", cy "18", r "3" ] []
        , Svg.path [ d "M18 9a9 9 0 0 1-9 9" ] []
        ]


gitCommit : Html msg
gitCommit =
    svgFeatherIcon "git-commit"
        [ Svg.circle [ cx "12", cy "12", r "4" ] []
        , Svg.line [ x1 "1.05", y1 "12", x2 "7", y2 "12" ] []
        , Svg.line [ x1 "17.01", y1 "12", x2 "22.96", y2 "12" ] []
        ]


gitMerge : Html msg
gitMerge =
    svgFeatherIcon "git-merge"
        [ Svg.circle [ cx "18", cy "18", r "3" ] []
        , Svg.circle [ cx "6", cy "6", r "3" ] []
        , Svg.path [ d "M6 21V9a9 9 0 0 0 9 9" ] []
        ]


gitPullRequest : Html msg
gitPullRequest =
    svgFeatherIcon "git-pull-request"
        [ Svg.circle [ cx "18", cy "18", r "3" ] []
        , Svg.circle [ cx "6", cy "6", r "3" ] []
        , Svg.path [ d "M13 6h3a2 2 0 0 1 2 2v7" ] []
        , Svg.line [ x1 "6", y1 "9", x2 "6", y2 "21" ] []
        ]


github : Html msg
github =
    svgFeatherIcon "github"
        [ Svg.path [ d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] []
        ]


grid : Html msg
grid =
    svgFeatherIcon "grid"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "7", height "7" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "3", width "7", height "7" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "14", width "7", height "7" ] []
        , Svg.rect [ Svg.Attributes.x "3", y "14", width "7", height "7" ] []
        ]


hash : Html msg
hash =
    svgFeatherIcon "hash"
        [ Svg.line [ x1 "4", y1 "9", x2 "20", y2 "9" ] []
        , Svg.line [ x1 "4", y1 "15", x2 "20", y2 "15" ] []
        , Svg.line [ x1 "10", y1 "3", x2 "8", y2 "21" ] []
        , Svg.line [ x1 "16", y1 "3", x2 "14", y2 "21" ] []
        ]


helpCircle : Html msg
helpCircle =
    svgFeatherIcon "help-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12", y2 "17" ] []
        ]


hexagon : Html msg
hexagon =
    svgFeatherIcon "hexagon"
        [ Svg.path [ d "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z" ] []
        ]


home : Html msg
home =
    svgFeatherIcon "home"
        [ Svg.path [ d "M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z" ] []
        , Svg.polyline [ points "9 22 9 12 15 12 15 22" ] []
        ]


italic : Html msg
italic =
    svgFeatherIcon "italic"
        [ Svg.line [ x1 "19", y1 "4", x2 "10", y2 "4" ] []
        , Svg.line [ x1 "14", y1 "20", x2 "5", y2 "20" ] []
        , Svg.line [ x1 "15", y1 "4", x2 "9", y2 "20" ] []
        ]


link : Html msg
link =
    svgFeatherIcon "link"
        [ Svg.path [ d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" ] []
        , Svg.path [ d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" ] []
        ]


loader : Html msg
loader =
    svgFeatherIcon "loader"
        [ Svg.line [ x1 "12", y1 "2", x2 "12", y2 "6" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "22" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "7.76", y2 "7.76" ] []
        , Svg.line [ x1 "16.24", y1 "16.24", x2 "19.07", y2 "19.07" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "6", y2 "12" ] []
        , Svg.line [ x1 "18", y1 "12", x2 "22", y2 "12" ] []
        , Svg.line [ x1 "4.93", y1 "19.07", x2 "7.76", y2 "16.24" ] []
        , Svg.line [ x1 "16.24", y1 "7.76", x2 "19.07", y2 "4.93" ] []
        ]


plusCircle : Html msg
plusCircle =
    svgFeatherIcon "plus-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


save : Html msg
save =
    svgFeatherIcon "save"
        [ Svg.path [ d "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z" ] []
        , Svg.polyline [ points "17 21 17 13 7 13 7 21" ] []
        , Svg.polyline [ points "7 3 7 8 15 8" ] []
        ]


search : Html msg
search =
    svgFeatherIcon "search"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        ]


send : Html msg
send =
    svgFeatherIcon "send"
        [ Svg.line [ x1 "22", y1 "2", x2 "11", y2 "13" ] []
        , Svg.polygon [ points "22 2 15 22 11 13 2 9 22 2" ] []
        ]


settings : Html msg
settings =
    svgFeatherIcon "settings"
        [ Svg.circle [ cx "12", cy "12", r "3" ] []
        , Svg.path [ d "M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z" ] []
        ]


share2 : Html msg
share2 =
    svgFeatherIcon "share-2"
        [ Svg.circle [ cx "18", cy "5", r "3" ] []
        , Svg.circle [ cx "6", cy "12", r "3" ] []
        , Svg.circle [ cx "18", cy "19", r "3" ] []
        , Svg.line [ x1 "8.59", y1 "13.51", x2 "15.42", y2 "17.49" ] []
        , Svg.line [ x1 "15.41", y1 "6.51", x2 "8.59", y2 "10.49" ] []
        ]


tag : Html msg
tag =
    svgFeatherIcon "tag"
        [ Svg.path [ d "M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z" ] []
        , Svg.line [ x1 "7", y1 "7", x2 "7", y2 "7" ] []
        ]


toggleLeft : Html msg
toggleLeft =
    svgFeatherIcon "toggle-left"
        [ Svg.rect [ Svg.Attributes.x "1", y "5", width "22", height "14", rx "7", ry "7" ] []
        , Svg.circle [ cx "8", cy "12", r "3" ] []
        ]


trash2 : Html msg
trash2 =
    svgFeatherIcon "trash-2"
        [ Svg.polyline [ points "3 6 5 6 21 6" ] []
        , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , Svg.line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , Svg.line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]


upload : Html msg
upload =
    svgFeatherIcon "upload"
        [ Svg.path [ d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" ] []
        , Svg.polyline [ points "17 8 12 3 7 8" ] []
        , Svg.line [ x1 "12", y1 "3", x2 "12", y2 "15" ] []
        ]
