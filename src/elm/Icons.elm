module Icons exposing
    ( activity
    , airplay
    , alertCircle
    , alertOctagon
    , alertTriangle
    , alignCenter
    , alignJustify
    , alignLeft
    , alignRight
    , anchor
    , aperture
    , archive
    , arrowDown
    , arrowDownCircle
    , arrowDownLeft
    , arrowDownRight
    , arrowLeft
    , arrowLeftCircle
    , arrowRight
    , arrowRightCircle
    , arrowUp
    , arrowUpCircle
    , arrowUpLeft
    , arrowUpRight
    , atSign
    , award
    , barChart2
    , bell
    , bellOff
    , bold
    , book
    , bookOpen
    , bookmark
    , box
    , briefcase
    , calendar
    , cast
    , check
    , checkCircle
    , checkSquare
    , chevronDown
    , chevronLeft
    , chevronRight
    , chevronUp
    , chevronsDown
    , chevronsLeft
    , chevronsRight
    , chevronsUp
    , chrome
    , circle
    , clipboard
    , clock
    , copy
    , database
    , delete
    , download
    , downloadCloud
    , edit
    , eye
    , eyeOff
    , file
    , filePlus
    , fileText
    , filter
    , folder
    , folderMinus
    , folderPlus
    , gitBranch
    , gitCommit
    , gitMerge
    , gitPullRequest
    , github
    , grid
    , hardDrive
    , hash
    , helpCircle
    , hexagon
    , home
    , image
    , info
    , italic
    , layers
    , layout
    , link
    , list
    , loader
    , loader2
    , mail
    , map
    , mapPin
    , maximize2
    , paperclip
    , play
    , playCircle
    , plusCircle
    , plusSquare
    , power
    , refreshCcw
    , refreshCw
    , repeat
    , rotateCcw
    , rotateCw
    , rss
    , save
    , search
    , send
    , server
    , settings
    , share
    , share2
    , shoppingCart
    , slash
    , smile
    , square
    , star
    , sun
    , tag
    , terminal
    , toggleLeft
    , toggleRight
    , trash2
    , trello
    , type_
    , underline
    , upload
    , user
    , userPlus
    , users
    , video
    , videoOff
    , volume2
    , wifi
    , x
    , youtube
    , zoomIn
    , zoomOut
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


activity : Html msg
activity =
    svgFeatherIcon "activity"
        [ Svg.polyline [ points "22 12 18 12 15 21 9 3 6 12 2 12" ] []
        ]


airplay : Html msg
airplay =
    svgFeatherIcon "airplay"
        [ Svg.path [ d "M5 17H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2h-1" ] []
        , Svg.polygon [ points "12 15 17 21 7 21 12 15" ] []
        ]


alertCircle : Html msg
alertCircle =
    svgFeatherIcon "alert-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "16" ] []
        ]


alertOctagon : Html msg
alertOctagon =
    svgFeatherIcon "alert-octagon"
        [ Svg.polygon [ points "7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "16" ] []
        ]


alertTriangle : Html msg
alertTriangle =
    svgFeatherIcon "alert-triangle"
        [ Svg.path [ d "M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z" ] []
        , Svg.line [ x1 "12", y1 "9", x2 "12", y2 "13" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12", y2 "17" ] []
        ]


alignCenter : Html msg
alignCenter =
    svgFeatherIcon "align-center"
        [ Svg.line [ x1 "18", y1 "10", x2 "6", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "18", y1 "18", x2 "6", y2 "18" ] []
        ]


alignJustify : Html msg
alignJustify =
    svgFeatherIcon "align-justify"
        [ Svg.line [ x1 "21", y1 "10", x2 "3", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "3", y2 "18" ] []
        ]


alignLeft : Html msg
alignLeft =
    svgFeatherIcon "align-left"
        [ Svg.line [ x1 "17", y1 "10", x2 "3", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "17", y1 "18", x2 "3", y2 "18" ] []
        ]


alignRight : Html msg
alignRight =
    svgFeatherIcon "align-right"
        [ Svg.line [ x1 "21", y1 "10", x2 "7", y2 "10" ] []
        , Svg.line [ x1 "21", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "21", y1 "14", x2 "3", y2 "14" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "7", y2 "18" ] []
        ]


anchor : Html msg
anchor =
    svgFeatherIcon "anchor"
        [ Svg.circle [ cx "12", cy "5", r "3" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "8" ] []
        , Svg.path [ d "M5 12H2a10 10 0 0 0 20 0h-3" ] []
        ]


aperture : Html msg
aperture =
    svgFeatherIcon "aperture"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "14.31", y1 "8", x2 "20.05", y2 "17.94" ] []
        , Svg.line [ x1 "9.69", y1 "8", x2 "21.17", y2 "8" ] []
        , Svg.line [ x1 "7.38", y1 "12", x2 "13.12", y2 "2.06" ] []
        , Svg.line [ x1 "9.69", y1 "16", x2 "3.95", y2 "6.06" ] []
        , Svg.line [ x1 "14.31", y1 "16", x2 "2.83", y2 "16" ] []
        , Svg.line [ x1 "16.62", y1 "12", x2 "10.88", y2 "21.94" ] []
        ]


archive : Html msg
archive =
    svgFeatherIcon "archive"
        [ Svg.polyline [ points "21 8 21 21 3 21 3 8" ] []
        , Svg.rect [ Svg.Attributes.x "1", y "3", width "22", height "5" ] []
        , Svg.line [ x1 "10", y1 "12", x2 "14", y2 "12" ] []
        ]


arrowDown : Html msg
arrowDown =
    svgFeatherIcon "arrow-down"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.polyline [ points "19 12 12 19 5 12" ] []
        ]


arrowDownCircle : Html msg
arrowDownCircle =
    svgFeatherIcon "arrow-down-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "8 12 12 16 16 12" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        ]


arrowDownLeft : Html msg
arrowDownLeft =
    svgFeatherIcon "arrow-down-left"
        [ Svg.line [ x1 "17", y1 "7", x2 "7", y2 "17" ] []
        , Svg.polyline [ points "17 17 7 17 7 7" ] []
        ]


arrowDownRight : Html msg
arrowDownRight =
    svgFeatherIcon "arrow-down-right"
        [ Svg.line [ x1 "7", y1 "7", x2 "17", y2 "17" ] []
        , Svg.polyline [ points "17 7 17 17 7 17" ] []
        ]


arrowLeft : Html msg
arrowLeft =
    svgFeatherIcon "arrow-left"
        [ Svg.line [ x1 "19", y1 "12", x2 "5", y2 "12" ] []
        , Svg.polyline [ points "12 19 5 12 12 5" ] []
        ]


arrowLeftCircle : Html msg
arrowLeftCircle =
    svgFeatherIcon "arrow-left-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 8 8 12 12 16" ] []
        , Svg.line [ x1 "16", y1 "12", x2 "8", y2 "12" ] []
        ]


arrowRight : Html msg
arrowRight =
    svgFeatherIcon "arrow-right"
        [ Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        , Svg.polyline [ points "12 5 19 12 12 19" ] []
        ]


arrowRightCircle : Html msg
arrowRightCircle =
    svgFeatherIcon "arrow-right-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "12 16 16 12 12 8" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


arrowUp : Html msg
arrowUp =
    svgFeatherIcon "arrow-up"
        [ Svg.line [ x1 "12", y1 "19", x2 "12", y2 "5" ] []
        , Svg.polyline [ points "5 12 12 5 19 12" ] []
        ]


arrowUpCircle : Html msg
arrowUpCircle =
    svgFeatherIcon "arrow-up-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polyline [ points "16 12 12 8 8 12" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "8" ] []
        ]


arrowUpLeft : Html msg
arrowUpLeft =
    svgFeatherIcon "arrow-up-left"
        [ Svg.line [ x1 "17", y1 "17", x2 "7", y2 "7" ] []
        , Svg.polyline [ points "7 17 7 7 17 7" ] []
        ]


arrowUpRight : Html msg
arrowUpRight =
    svgFeatherIcon "arrow-up-right"
        [ Svg.line [ x1 "7", y1 "17", x2 "17", y2 "7" ] []
        , Svg.polyline [ points "7 7 17 7 17 17" ] []
        ]


atSign : Html msg
atSign =
    svgFeatherIcon "at-sign"
        [ Svg.circle [ cx "12", cy "12", r "4" ] []
        , Svg.path [ d "M16 8v5a3 3 0 0 0 6 0v-1a10 10 0 1 0-3.92 7.94" ] []
        ]


award : Html msg
award =
    svgFeatherIcon "award"
        [ Svg.circle [ cx "12", cy "8", r "7" ] []
        , Svg.polyline [ points "8.21 13.89 7 23 12 20 17 23 15.79 13.88" ] []
        ]


barChart2 : Html msg
barChart2 =
    svgFeatherIcon "bar-chart-2"
        [ Svg.line [ x1 "18", y1 "20", x2 "18", y2 "10" ] []
        , Svg.line [ x1 "12", y1 "20", x2 "12", y2 "4" ] []
        , Svg.line [ x1 "6", y1 "20", x2 "6", y2 "14" ] []
        ]


bell : Html msg
bell =
    svgFeatherIcon "bell"
        [ Svg.path [ d "M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9" ] []
        , Svg.path [ d "M13.73 21a2 2 0 0 1-3.46 0" ] []
        ]


bellOff : Html msg
bellOff =
    svgFeatherIcon "bell-off"
        [ Svg.path [ d "M13.73 21a2 2 0 0 1-3.46 0" ] []
        , Svg.path [ d "M18.63 13A17.89 17.89 0 0 1 18 8" ] []
        , Svg.path [ d "M6.26 6.26A5.86 5.86 0 0 0 6 8c0 7-3 9-3 9h14" ] []
        , Svg.path [ d "M18 8a6 6 0 0 0-9.33-5" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


bold : Html msg
bold =
    svgFeatherIcon "bold"
        [ Svg.path [ d "M6 4h8a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z" ] []
        , Svg.path [ d "M6 12h9a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z" ] []
        ]


book : Html msg
book =
    svgFeatherIcon "book"
        [ Svg.path [ d "M4 19.5A2.5 2.5 0 0 1 6.5 17H20" ] []
        , Svg.path [ d "M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z" ] []
        ]


bookOpen : Html msg
bookOpen =
    svgFeatherIcon "book-open"
        [ Svg.path [ d "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z" ] []
        , Svg.path [ d "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z" ] []
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


briefcase : Html msg
briefcase =
    svgFeatherIcon "briefcase"
        [ Svg.rect [ Svg.Attributes.x "2", y "7", width "20", height "14", rx "2", ry "2" ] []
        , Svg.path [ d "M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16" ] []
        ]


calendar : Html msg
calendar =
    svgFeatherIcon "calendar"
        [ Svg.rect [ Svg.Attributes.x "3", y "4", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "16", y1 "2", x2 "16", y2 "6" ] []
        , Svg.line [ x1 "8", y1 "2", x2 "8", y2 "6" ] []
        , Svg.line [ x1 "3", y1 "10", x2 "21", y2 "10" ] []
        ]


cast : Html msg
cast =
    svgFeatherIcon "cast"
        [ Svg.path [ d "M2 16.1A5 5 0 0 1 5.9 20M2 12.05A9 9 0 0 1 9.95 20M2 8V6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2h-6" ] []
        , Svg.line [ x1 "2", y1 "20", x2 "2", y2 "20" ] []
        ]


check : Html msg
check =
    svgFeatherIcon "check"
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


checkCircle : Html msg
checkCircle =
    svgFeatherIcon "check-circle"
        [ Svg.path [ d "M22 11.08V12a10 10 0 1 1-5.93-9.14" ] []
        , Svg.polyline [ points "22 4 12 14.01 9 11.01" ] []
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


chevronsDown : Html msg
chevronsDown =
    svgFeatherIcon "chevrons-down"
        [ Svg.polyline [ points "7 13 12 18 17 13" ] []
        , Svg.polyline [ points "7 6 12 11 17 6" ] []
        ]


chevronsLeft : Html msg
chevronsLeft =
    svgFeatherIcon "chevrons-left"
        [ Svg.polyline [ points "11 17 6 12 11 7" ] []
        , Svg.polyline [ points "18 17 13 12 18 7" ] []
        ]


chevronsRight : Html msg
chevronsRight =
    svgFeatherIcon "chevrons-right"
        [ Svg.polyline [ points "13 17 18 12 13 7" ] []
        , Svg.polyline [ points "6 17 11 12 6 7" ] []
        ]


chevronsUp : Html msg
chevronsUp =
    svgFeatherIcon "chevrons-up"
        [ Svg.polyline [ points "17 11 12 6 7 11" ] []
        , Svg.polyline [ points "17 18 12 13 7 18" ] []
        ]


chrome : Html msg
chrome =
    svgFeatherIcon "chrome"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.circle [ cx "12", cy "12", r "4" ] []
        , Svg.line [ x1 "21.17", y1 "8", x2 "12", y2 "8" ] []
        , Svg.line [ x1 "3.95", y1 "6.06", x2 "8.54", y2 "14" ] []
        , Svg.line [ x1 "10.88", y1 "21.94", x2 "15.46", y2 "14" ] []
        ]


circle : Html msg
circle =
    svgFeatherIcon "circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
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


downloadCloud : Html msg
downloadCloud =
    svgFeatherIcon "download-cloud"
        [ Svg.polyline [ points "8 17 12 21 16 17" ] []
        , Svg.line [ x1 "12", y1 "12", x2 "12", y2 "21" ] []
        , Svg.path [ d "M20.88 18.09A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.29" ] []
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


eyeOff : Html msg
eyeOff =
    svgFeatherIcon "eye-off"
        [ Svg.path [ d "M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


file : Html msg
file =
    svgFeatherIcon "file"
        [ Svg.path [ d "M13 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z" ] []
        , Svg.polyline [ points "13 2 13 9 20 9" ] []
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


filter : Html msg
filter =
    svgFeatherIcon "filter"
        [ Svg.polygon [ points "22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3" ] []
        ]


folder : Html msg
folder =
    svgFeatherIcon "folder"
        [ Svg.path [ d "M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z" ] []
        ]


folderMinus : Html msg
folderMinus =
    svgFeatherIcon "folder-minus"
        [ Svg.path [ d "M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z" ] []
        , Svg.line [ x1 "9", y1 "14", x2 "15", y2 "14" ] []
        ]


folderPlus : Html msg
folderPlus =
    svgFeatherIcon "folder-plus"
        [ Svg.path [ d "M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z" ] []
        , Svg.line [ x1 "12", y1 "11", x2 "12", y2 "17" ] []
        , Svg.line [ x1 "9", y1 "14", x2 "15", y2 "14" ] []
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


hardDrive : Html msg
hardDrive =
    svgFeatherIcon "hard-drive"
        [ Svg.line [ x1 "22", y1 "12", x2 "2", y2 "12" ] []
        , Svg.path [ d "M5.45 5.11L2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z" ] []
        , Svg.line [ x1 "6", y1 "16", x2 "6", y2 "16" ] []
        , Svg.line [ x1 "10", y1 "16", x2 "10", y2 "16" ] []
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


image : Html msg
image =
    svgFeatherIcon "image"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.circle [ cx "8.5", cy "8.5", r "1.5" ] []
        , Svg.polyline [ points "21 15 16 10 5 21" ] []
        ]


info : Html msg
info =
    svgFeatherIcon "info"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "8" ] []
        ]


italic : Html msg
italic =
    svgFeatherIcon "italic"
        [ Svg.line [ x1 "19", y1 "4", x2 "10", y2 "4" ] []
        , Svg.line [ x1 "14", y1 "20", x2 "5", y2 "20" ] []
        , Svg.line [ x1 "15", y1 "4", x2 "9", y2 "20" ] []
        ]


layers : Html msg
layers =
    svgFeatherIcon "layers"
        [ Svg.polygon [ points "12 2 2 7 12 12 22 7 12 2" ] []
        , Svg.polyline [ points "2 17 12 22 22 17" ] []
        , Svg.polyline [ points "2 12 12 17 22 12" ] []
        ]


layout : Html msg
layout =
    svgFeatherIcon "layout"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "3", y1 "9", x2 "21", y2 "9" ] []
        , Svg.line [ x1 "9", y1 "21", x2 "9", y2 "9" ] []
        ]


link : Html msg
link =
    svgFeatherIcon "link"
        [ Svg.path [ d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" ] []
        , Svg.path [ d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" ] []
        ]


list : Html msg
list =
    svgFeatherIcon "list"
        [ Svg.line [ x1 "8", y1 "6", x2 "21", y2 "6" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "21", y2 "12" ] []
        , Svg.line [ x1 "8", y1 "18", x2 "21", y2 "18" ] []
        , Svg.line [ x1 "3", y1 "6", x2 "3", y2 "6" ] []
        , Svg.line [ x1 "3", y1 "12", x2 "3", y2 "12" ] []
        , Svg.line [ x1 "3", y1 "18", x2 "3", y2 "18" ] []
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


loader2 : Html msg
loader2 =
    svgFeatherIcon "loader2"
        [ svg
            [ width "200", height "200", viewBox "0 0 100 100", preserveAspectRatio "xMidYMid", class "lds-wedges", Svg.Attributes.style "background: none;" ]
            [ g [ transform "translate(50,50)" ]
                [ g [ transform "scale(0.7)" ]
                    [ g [ transform "translate(-50,-50)" ]
                        [ g [ transform "rotate(267.003 50 50)" ] [ animateTransform [ attributeName "transform", calcMode "linear", values "0 50 50;360 50 50", keyTimes "0;1", dur "0.75s", begin "0s", repeatCount "indefinite" ] [], Svg.path [ d "M50 50L50 0A50 50 0 0 1 100 50Z", fillOpacity "0.8", fill "#fcb711" ] [] ], g [ transform "rotate(110.253 50 50)" ] [ animateTransform [ attributeName "transform", calcMode "linear", values "0 50 50;360 50 50", keyTimes "0;1", dur "1s", begin "0s", repeatCount "indefinite" ] [], Svg.path [ d "M50 50L50 0A50 50 0 0 1 100 50Z", transform "rotate(90 50 50)", fillOpacity "0.8", fill "#f37021" ] [] ], g [ transform "rotate(313.502 50 50)" ] [ animateTransform [ attributeName "transform", calcMode "linear", values "0 50 50;360 50 50", keyTimes "0;1", dur "1.5s", begin "0s", repeatCount "indefinite" ] [], Svg.path [ d "M50 50L50 0A50 50 0 0 1 100 50Z", transform "rotate(180 50 50)", fillOpacity "0.8", fill "#cc004c" ] [] ], g [ transform "rotate(156.751 50 50)" ] [ animateTransform [ attributeName "transform", calcMode "linear", values "0 50 50;360 50 50", keyTimes "0;1", dur "3s", begin "0s", repeatCount "indefinite" ] [], Svg.path [ d "M50 50L50 0A50 50 0 0 1 100 50Z", transform "rotate(270 50 50)", fillOpacity "0.8", fill "#6460aa" ] [] ] ]
                    ]
                ]
            ]
        ]


mail : Html msg
mail =
    svgFeatherIcon "mail"
        [ Svg.path [ d "M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z" ] []
        , Svg.polyline [ points "22,6 12,13 2,6" ] []
        ]


map : Html msg
map =
    svgFeatherIcon "map"
        [ Svg.polygon [ points "1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6" ] []
        , Svg.line [ x1 "8", y1 "2", x2 "8", y2 "18" ] []
        , Svg.line [ x1 "16", y1 "6", x2 "16", y2 "22" ] []
        ]


mapPin : Html msg
mapPin =
    svgFeatherIcon "map-pin"
        [ Svg.path [ d "M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z" ] []
        , Svg.circle [ cx "12", cy "10", r "3" ] []
        ]


maximize2 : Html msg
maximize2 =
    svgFeatherIcon "maximize-2"
        [ Svg.polyline [ points "15 3 21 3 21 9" ] []
        , Svg.polyline [ points "9 21 3 21 3 15" ] []
        , Svg.line [ x1 "21", y1 "3", x2 "14", y2 "10" ] []
        , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
        ]


paperclip : Html msg
paperclip =
    svgFeatherIcon "paperclip"
        [ Svg.path [ d "M21.44 11.05l-9.19 9.19a6 6 0 0 1-8.49-8.49l9.19-9.19a4 4 0 0 1 5.66 5.66l-9.2 9.19a2 2 0 0 1-2.83-2.83l8.49-8.48" ] []
        ]


play : Html msg
play =
    svgFeatherIcon "play"
        [ Svg.polygon [ points "5 3 19 12 5 21 5 3" ] []
        ]


playCircle : Html msg
playCircle =
    svgFeatherIcon "play-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polygon [ points "10 8 16 12 10 16 10 8" ] []
        ]


plusCircle : Html msg
plusCircle =
    svgFeatherIcon "plus-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


plusSquare : Html msg
plusSquare =
    svgFeatherIcon "plus-square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


power : Html msg
power =
    svgFeatherIcon "power"
        [ Svg.path [ d "M18.36 6.64a9 9 0 1 1-12.73 0" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "12" ] []
        ]


refreshCcw : Html msg
refreshCcw =
    svgFeatherIcon "refresh-ccw"
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.polyline [ points "23 20 23 14 17 14" ] []
        , Svg.path [ d "M20.49 9A9 9 0 0 0 5.64 5.64L1 10m22 4l-4.64 4.36A9 9 0 0 1 3.51 15" ] []
        ]


refreshCw : Html msg
refreshCw =
    svgFeatherIcon "refresh-cw"
        [ Svg.polyline [ points "23 4 23 10 17 10" ] []
        , Svg.polyline [ points "1 20 1 14 7 14" ] []
        , Svg.path [ d "M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15" ] []
        ]


repeat : Html msg
repeat =
    svgFeatherIcon "repeat"
        [ Svg.polyline [ points "17 1 21 5 17 9" ] []
        , Svg.path [ d "M3 11V9a4 4 0 0 1 4-4h14" ] []
        , Svg.polyline [ points "7 23 3 19 7 15" ] []
        , Svg.path [ d "M21 13v2a4 4 0 0 1-4 4H3" ] []
        ]


rotateCcw : Html msg
rotateCcw =
    svgFeatherIcon "rotate-ccw"
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
        ]


rotateCw : Html msg
rotateCw =
    svgFeatherIcon "rotate-cw"
        [ Svg.polyline [ points "23 4 23 10 17 10" ] []
        , Svg.path [ d "M20.49 15a9 9 0 1 1-2.12-9.36L23 10" ] []
        ]


rss : Html msg
rss =
    svgFeatherIcon "rss"
        [ Svg.path [ d "M4 11a9 9 0 0 1 9 9" ] []
        , Svg.path [ d "M4 4a16 16 0 0 1 16 16" ] []
        , Svg.circle [ cx "5", cy "19", r "1" ] []
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


server : Html msg
server =
    svgFeatherIcon "server"
        [ Svg.rect [ Svg.Attributes.x "2", y "2", width "20", height "8", rx "2", ry "2" ] []
        , Svg.rect [ Svg.Attributes.x "2", y "14", width "20", height "8", rx "2", ry "2" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "6", y2 "6" ] []
        , Svg.line [ x1 "6", y1 "18", x2 "6", y2 "18" ] []
        ]


settings : Html msg
settings =
    svgFeatherIcon "settings"
        [ Svg.circle [ cx "12", cy "12", r "3" ] []
        , Svg.path [ d "M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z" ] []
        ]


share : Html msg
share =
    svgFeatherIcon "share"
        [ Svg.path [ d "M4 12v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8" ] []
        , Svg.polyline [ points "16 6 12 2 8 6" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "15" ] []
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


shoppingCart : Html msg
shoppingCart =
    svgFeatherIcon "shopping-cart"
        [ Svg.circle [ cx "9", cy "21", r "1" ] []
        , Svg.circle [ cx "20", cy "21", r "1" ] []
        , Svg.path [ d "M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6" ] []
        ]


slash : Html msg
slash =
    svgFeatherIcon "slash"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "19.07", y2 "19.07" ] []
        ]


smile : Html msg
smile =
    svgFeatherIcon "smile"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M8 14s1.5 2 4 2 4-2 4-2" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "9.01", y2 "9" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "15.01", y2 "9" ] []
        ]


square : Html msg
square =
    svgFeatherIcon "square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        ]


star : Html msg
star =
    svgFeatherIcon "star"
        [ Svg.polygon [ points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2" ] []
        ]


sun : Html msg
sun =
    svgFeatherIcon "sun"
        [ Svg.circle [ cx "12", cy "12", r "5" ] []
        , Svg.line [ x1 "12", y1 "1", x2 "12", y2 "3" ] []
        , Svg.line [ x1 "12", y1 "21", x2 "12", y2 "23" ] []
        , Svg.line [ x1 "4.22", y1 "4.22", x2 "5.64", y2 "5.64" ] []
        , Svg.line [ x1 "18.36", y1 "18.36", x2 "19.78", y2 "19.78" ] []
        , Svg.line [ x1 "1", y1 "12", x2 "3", y2 "12" ] []
        , Svg.line [ x1 "21", y1 "12", x2 "23", y2 "12" ] []
        , Svg.line [ x1 "4.22", y1 "19.78", x2 "5.64", y2 "18.36" ] []
        , Svg.line [ x1 "18.36", y1 "5.64", x2 "19.78", y2 "4.22" ] []
        ]


tag : Html msg
tag =
    svgFeatherIcon "tag"
        [ Svg.path [ d "M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z" ] []
        , Svg.line [ x1 "7", y1 "7", x2 "7", y2 "7" ] []
        ]


terminal : Html msg
terminal =
    svgFeatherIcon "terminal"
        [ Svg.polyline [ points "4 17 10 11 4 5" ] []
        , Svg.line [ x1 "12", y1 "19", x2 "20", y2 "19" ] []
        ]


toggleLeft : Html msg
toggleLeft =
    svgFeatherIcon "toggle-left"
        [ Svg.rect [ Svg.Attributes.x "1", y "5", width "22", height "14", rx "7", ry "7" ] []
        , Svg.circle [ cx "8", cy "12", r "3" ] []
        ]


toggleRight : Html msg
toggleRight =
    svgFeatherIcon "toggle-right"
        [ Svg.rect [ Svg.Attributes.x "1", y "5", width "22", height "14", rx "7", ry "7" ] []
        , Svg.circle [ cx "16", cy "12", r "3" ] []
        ]


trash2 : Html msg
trash2 =
    svgFeatherIcon "trash-2"
        [ Svg.polyline [ points "3 6 5 6 21 6" ] []
        , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , Svg.line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , Svg.line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]


trello : Html msg
trello =
    svgFeatherIcon "trello"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.rect [ Svg.Attributes.x "7", y "7", width "3", height "9" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "7", width "3", height "5" ] []
        ]


type_ : Html msg
type_ =
    svgFeatherIcon "type"
        [ Svg.polyline [ points "4 7 4 4 20 4 20 7" ] []
        , Svg.line [ x1 "9", y1 "20", x2 "15", y2 "20" ] []
        , Svg.line [ x1 "12", y1 "4", x2 "12", y2 "20" ] []
        ]


underline : Html msg
underline =
    svgFeatherIcon "underline"
        [ Svg.path [ d "M6 3v7a6 6 0 0 0 6 6 6 6 0 0 0 6-6V3" ] []
        , Svg.line [ x1 "4", y1 "21", x2 "20", y2 "21" ] []
        ]


upload : Html msg
upload =
    svgFeatherIcon "upload"
        [ Svg.path [ d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" ] []
        , Svg.polyline [ points "17 8 12 3 7 8" ] []
        , Svg.line [ x1 "12", y1 "3", x2 "12", y2 "15" ] []
        ]


user : Html msg
user =
    svgFeatherIcon "user"
        [ Svg.path [ d "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "12", cy "7", r "4" ] []
        ]


userPlus : Html msg
userPlus =
    svgFeatherIcon "user-plus"
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "8.5", cy "7", r "4" ] []
        , Svg.line [ x1 "20", y1 "8", x2 "20", y2 "14" ] []
        , Svg.line [ x1 "23", y1 "11", x2 "17", y2 "11" ] []
        ]


users : Html msg
users =
    svgFeatherIcon "users"
        [ Svg.path [ d "M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "9", cy "7", r "4" ] []
        , Svg.path [ d "M23 21v-2a4 4 0 0 0-3-3.87" ] []
        , Svg.path [ d "M16 3.13a4 4 0 0 1 0 7.75" ] []
        ]


video : Html msg
video =
    svgFeatherIcon "video"
        [ Svg.polygon [ points "23 7 16 12 23 17 23 7" ] []
        , Svg.rect [ Svg.Attributes.x "1", y "5", width "15", height "14", rx "2", ry "2" ] []
        ]


videoOff : Html msg
videoOff =
    svgFeatherIcon "video-off"
        [ Svg.path [ d "M16 16v1a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V7a2 2 0 0 1 2-2h2m5.66 0H14a2 2 0 0 1 2 2v3.34l1 1L23 7v10" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


volume2 : Html msg
volume2 =
    svgFeatherIcon "volume-2"
        [ Svg.polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , Svg.path [ d "M19.07 4.93a10 10 0 0 1 0 14.14M15.54 8.46a5 5 0 0 1 0 7.07" ] []
        ]


wifi : Html msg
wifi =
    svgFeatherIcon "wifi"
        [ Svg.path [ d "M5 12.55a11 11 0 0 1 14.08 0" ] []
        , Svg.path [ d "M1.42 9a16 16 0 0 1 21.16 0" ] []
        , Svg.path [ d "M8.53 16.11a6 6 0 0 1 6.95 0" ] []
        , Svg.line [ x1 "12", y1 "20", x2 "12", y2 "20" ] []
        ]


x : Html msg
x =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


youtube : Html msg
youtube =
    svgFeatherIcon "youtube"
        [ Svg.path [ d "M22.54 6.42a2.78 2.78 0 0 0-1.94-2C18.88 4 12 4 12 4s-6.88 0-8.6.46a2.78 2.78 0 0 0-1.94 2A29 29 0 0 0 1 11.75a29 29 0 0 0 .46 5.33A2.78 2.78 0 0 0 3.4 19c1.72.46 8.6.46 8.6.46s6.88 0 8.6-.46a2.78 2.78 0 0 0 1.94-2 29 29 0 0 0 .46-5.25 29 29 0 0 0-.46-5.33z" ] []
        , Svg.polygon [ points "9.75 15.02 15.5 11.75 9.75 8.48 9.75 15.02" ] []
        ]


zoomIn : Html msg
zoomIn =
    svgFeatherIcon "zoom-in"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        , Svg.line [ x1 "11", y1 "8", x2 "11", y2 "14" ] []
        , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
        ]


zoomOut : Html msg
zoomOut =
    svgFeatherIcon "zoom-out"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        , Svg.line [ x1 "8", y1 "11", x2 "14", y2 "11" ] []
        ]
