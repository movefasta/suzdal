module UI.Icons exposing
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
    , barChart
    , barChart2
    , battery
    , batteryCharging
    , bell
    , bellOff
    , bluetooth
    , bold
    , book
    , bookOpen
    , bookmark
    , box
    , briefcase
    , calendar
    , camera
    , cameraOff
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
    , cloud
    , cloudDrizzle
    , cloudLightning
    , cloudOff
    , cloudRain
    , cloudSnow
    , code
    , codepen
    , codesandbox
    , coffee
    , columns
    , command
    , compass
    , copy
    , cornerDownLeft
    , cornerDownRight
    , cornerLeftDown
    , cornerLeftUp
    , cornerRightDown
    , cornerRightUp
    , cornerUpLeft
    , cornerUpRight
    , cpu
    , creditCard
    , crop
    , crosshair
    , database
    , delete
    , disc
    , dollarSign
    , download
    , downloadCloud
    , droplet
    , edit
    , edit2
    , edit3
    , externalLink
    , eye
    , eyeOff
    , facebook
    , fastForward
    , feather
    , figma
    , file
    , fileMinus
    , filePlus
    , fileText
    , film
    , filter
    , flag
    , folder
    , folderMinus
    , folderPlus
    , framer
    , frown
    , gift
    , gitBranch
    , gitCommit
    , gitMerge
    , gitPullRequest
    , github
    , gitlab
    , globe
    , grid
    , hardDrive
    , hash
    , headphones
    , heart
    , helpCircle
    , hexagon
    , home
    , image
    , inbox
    , info
    , instagram
    , italic
    , key
    , layers
    , layout
    , lifeBuoy
    , link
    , link2
    , linkedin
    , list
    , loader
    , lock
    , logIn
    , logOut
    , mail
    , map
    , mapPin
    , maximize
    , maximize2
    , meh
    , menu
    , messageCircle
    , messageSquare
    , mic
    , micOff
    , minimize
    , minimize2
    , minus
    , minusCircle
    , minusSquare
    , monitor
    , moon
    , moreHorizontal
    , moreVertical
    , mousePointer
    , move
    , music
    , navigation
    , navigation2
    , octagon
    , package
    , paperclip
    , pause
    , pauseCircle
    , penTool
    , percent
    , phone
    , phoneCall
    , phoneForwarded
    , phoneIncoming
    , phoneMissed
    , phoneOff
    , phoneOutgoing
    , pieChart
    , play
    , playCircle
    , plus
    , plusCircle
    , plusSquare
    , pocket
    , power
    , printer
    , radio
    , refreshCcw
    , refreshCw
    , repeat
    , rewind
    , rotateCcw
    , rotateCw
    , rss
    , save
    , scissors
    , search
    , send
    , server
    , settings
    , share
    , share2
    , shield
    , shieldOff
    , shoppingBag
    , shoppingCart
    , shuffle
    , sidebar
    , skipBack
    , skipForward
    , slack
    , slash
    , sliders
    , smartphone
    , smile
    , speaker
    , square
    , star
    , stopCircle
    , sun
    , sunrise
    , sunset
    , tablet
    , tag
    , target
    , terminal
    , thermometer
    , thumbsDown
    , thumbsUp
    , toggleLeft
    , toggleRight
    , tool
    , trash
    , trash2
    , trello
    , trendingDown
    , trendingUp
    , triangle
    , truck
    , tv
    , twitter
    , type_
    , umbrella
    , underline
    , unlock
    , upload
    , uploadCloud
    , user
    , userCheck
    , userMinus
    , userPlus
    , userX
    , users
    , video
    , videoOff
    , voicemail
    , volume
    , volume1
    , volume2
    , volumeX
    , watch
    , wifi
    , wifiOff
    , wind
    , x
    , xCircle
    , xOctagon
    , xSquare
    , youtube
    , zap
    , zapOff
    , zoomIn
    , zoomOut
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "1"
        , viewBox "0 0 24 24"
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


barChart : Html msg
barChart =
    svgFeatherIcon "bar-chart"
        [ Svg.line [ x1 "12", y1 "20", x2 "12", y2 "10" ] []
        , Svg.line [ x1 "18", y1 "20", x2 "18", y2 "4" ] []
        , Svg.line [ x1 "6", y1 "20", x2 "6", y2 "16" ] []
        ]


barChart2 : Html msg
barChart2 =
    svgFeatherIcon "bar-chart-2"
        [ Svg.line [ x1 "18", y1 "20", x2 "18", y2 "10" ] []
        , Svg.line [ x1 "12", y1 "20", x2 "12", y2 "4" ] []
        , Svg.line [ x1 "6", y1 "20", x2 "6", y2 "14" ] []
        ]


battery : Html msg
battery =
    svgFeatherIcon "battery"
        [ Svg.rect [ Svg.Attributes.x "1", y "6", width "18", height "12", rx "2", ry "2" ] []
        , Svg.line [ x1 "23", y1 "13", x2 "23", y2 "11" ] []
        ]


batteryCharging : Html msg
batteryCharging =
    svgFeatherIcon "battery-charging"
        [ Svg.path [ d "M5 18H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h3.19M15 6h2a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2h-3.19" ] []
        , Svg.line [ x1 "23", y1 "13", x2 "23", y2 "11" ] []
        , Svg.polyline [ points "11 6 7 12 13 12 9 18" ] []
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


bluetooth : Html msg
bluetooth =
    svgFeatherIcon "bluetooth"
        [ Svg.polyline [ points "6.5 6.5 17.5 17.5 12 23 12 1 17.5 6.5 6.5 17.5" ] []
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


camera : Html msg
camera =
    svgFeatherIcon "camera"
        [ Svg.path [ d "M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z" ] []
        , Svg.circle [ cx "12", cy "13", r "4" ] []
        ]


cameraOff : Html msg
cameraOff =
    svgFeatherIcon "camera-off"
        [ Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        , Svg.path [ d "M21 21H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h3m3-3h6l2 3h4a2 2 0 0 1 2 2v9.34m-7.72-2.06a4 4 0 1 1-5.56-5.56" ] []
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


cloud : Html msg
cloud =
    svgFeatherIcon "cloud"
        [ Svg.path [ d "M18 10h-1.26A8 8 0 1 0 9 20h9a5 5 0 0 0 0-10z" ] []
        ]


cloudDrizzle : Html msg
cloudDrizzle =
    svgFeatherIcon "cloud-drizzle"
        [ Svg.line [ x1 "8", y1 "19", x2 "8", y2 "21" ] []
        , Svg.line [ x1 "8", y1 "13", x2 "8", y2 "15" ] []
        , Svg.line [ x1 "16", y1 "19", x2 "16", y2 "21" ] []
        , Svg.line [ x1 "16", y1 "13", x2 "16", y2 "15" ] []
        , Svg.line [ x1 "12", y1 "21", x2 "12", y2 "23" ] []
        , Svg.line [ x1 "12", y1 "15", x2 "12", y2 "17" ] []
        , Svg.path [ d "M20 16.58A5 5 0 0 0 18 7h-1.26A8 8 0 1 0 4 15.25" ] []
        ]


cloudLightning : Html msg
cloudLightning =
    svgFeatherIcon "cloud-lightning"
        [ Svg.path [ d "M19 16.9A5 5 0 0 0 18 7h-1.26a8 8 0 1 0-11.62 9" ] []
        , Svg.polyline [ points "13 11 9 17 15 17 11 23" ] []
        ]


cloudOff : Html msg
cloudOff =
    svgFeatherIcon "cloud-off"
        [ Svg.path [ d "M22.61 16.95A5 5 0 0 0 18 10h-1.26a8 8 0 0 0-7.05-6M5 5a8 8 0 0 0 4 15h9a5 5 0 0 0 1.7-.3" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


cloudRain : Html msg
cloudRain =
    svgFeatherIcon "cloud-rain"
        [ Svg.line [ x1 "16", y1 "13", x2 "16", y2 "21" ] []
        , Svg.line [ x1 "8", y1 "13", x2 "8", y2 "21" ] []
        , Svg.line [ x1 "12", y1 "15", x2 "12", y2 "23" ] []
        , Svg.path [ d "M20 16.58A5 5 0 0 0 18 7h-1.26A8 8 0 1 0 4 15.25" ] []
        ]


cloudSnow : Html msg
cloudSnow =
    svgFeatherIcon "cloud-snow"
        [ Svg.path [ d "M20 17.58A5 5 0 0 0 18 8h-1.26A8 8 0 1 0 4 16.25" ] []
        , Svg.line [ x1 "8", y1 "16", x2 "8", y2 "16" ] []
        , Svg.line [ x1 "8", y1 "20", x2 "8", y2 "20" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "18" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "22" ] []
        , Svg.line [ x1 "16", y1 "16", x2 "16", y2 "16" ] []
        , Svg.line [ x1 "16", y1 "20", x2 "16", y2 "20" ] []
        ]


code : Html msg
code =
    svgFeatherIcon "code"
        [ Svg.polyline [ points "16 18 22 12 16 6" ] []
        , Svg.polyline [ points "8 6 2 12 8 18" ] []
        ]


codepen : Html msg
codepen =
    svgFeatherIcon "codepen"
        [ Svg.polygon [ points "12 2 22 8.5 22 15.5 12 22 2 15.5 2 8.5 12 2" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "15.5" ] []
        , Svg.polyline [ points "22 8.5 12 15.5 2 8.5" ] []
        , Svg.polyline [ points "2 15.5 12 8.5 22 15.5" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "8.5" ] []
        ]


codesandbox : Html msg
codesandbox =
    svgFeatherIcon "codesandbox"
        [ Svg.path [ d "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z" ] []
        , Svg.polyline [ points "7.5 4.21 12 6.81 16.5 4.21" ] []
        , Svg.polyline [ points "7.5 19.79 7.5 14.6 3 12" ] []
        , Svg.polyline [ points "21 12 16.5 14.6 16.5 19.79" ] []
        , Svg.polyline [ points "3.27 6.96 12 12.01 20.73 6.96" ] []
        , Svg.line [ x1 "12", y1 "22.08", x2 "12", y2 "12" ] []
        ]


coffee : Html msg
coffee =
    svgFeatherIcon "coffee"
        [ Svg.path [ d "M18 8h1a4 4 0 0 1 0 8h-1" ] []
        , Svg.path [ d "M2 8h16v9a4 4 0 0 1-4 4H6a4 4 0 0 1-4-4V8z" ] []
        , Svg.line [ x1 "6", y1 "1", x2 "6", y2 "4" ] []
        , Svg.line [ x1 "10", y1 "1", x2 "10", y2 "4" ] []
        , Svg.line [ x1 "14", y1 "1", x2 "14", y2 "4" ] []
        ]


columns : Html msg
columns =
    svgFeatherIcon "columns"
        [ Svg.path [ d "M12 3h7a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-7m0-18H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h7m0-18v18" ] []
        ]


command : Html msg
command =
    svgFeatherIcon "command"
        [ Svg.path [ d "M18 3a3 3 0 0 0-3 3v12a3 3 0 0 0 3 3 3 3 0 0 0 3-3 3 3 0 0 0-3-3H6a3 3 0 0 0-3 3 3 3 0 0 0 3 3 3 3 0 0 0 3-3V6a3 3 0 0 0-3-3 3 3 0 0 0-3 3 3 3 0 0 0 3 3h12a3 3 0 0 0 3-3 3 3 0 0 0-3-3z" ] []
        ]


compass : Html msg
compass =
    svgFeatherIcon "compass"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.polygon [ points "16.24 7.76 14.12 14.12 7.76 16.24 9.88 9.88 16.24 7.76" ] []
        ]


copy : Html msg
copy =
    svgFeatherIcon "copy"
        [ Svg.rect [ Svg.Attributes.x "9", y "9", width "13", height "13", rx "2", ry "2" ] []
        , Svg.path [ d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" ] []
        ]


cornerDownLeft : Html msg
cornerDownLeft =
    svgFeatherIcon "corner-down-left"
        [ Svg.polyline [ points "9 10 4 15 9 20" ] []
        , Svg.path [ d "M20 4v7a4 4 0 0 1-4 4H4" ] []
        ]


cornerDownRight : Html msg
cornerDownRight =
    svgFeatherIcon "corner-down-right"
        [ Svg.polyline [ points "15 10 20 15 15 20" ] []
        , Svg.path [ d "M4 4v7a4 4 0 0 0 4 4h12" ] []
        ]


cornerLeftDown : Html msg
cornerLeftDown =
    svgFeatherIcon "corner-left-down"
        [ Svg.polyline [ points "14 15 9 20 4 15" ] []
        , Svg.path [ d "M20 4h-7a4 4 0 0 0-4 4v12" ] []
        ]


cornerLeftUp : Html msg
cornerLeftUp =
    svgFeatherIcon "corner-left-up"
        [ Svg.polyline [ points "14 9 9 4 4 9" ] []
        , Svg.path [ d "M20 20h-7a4 4 0 0 1-4-4V4" ] []
        ]


cornerRightDown : Html msg
cornerRightDown =
    svgFeatherIcon "corner-right-down"
        [ Svg.polyline [ points "10 15 15 20 20 15" ] []
        , Svg.path [ d "M4 4h7a4 4 0 0 1 4 4v12" ] []
        ]


cornerRightUp : Html msg
cornerRightUp =
    svgFeatherIcon "corner-right-up"
        [ Svg.polyline [ points "10 9 15 4 20 9" ] []
        , Svg.path [ d "M4 20h7a4 4 0 0 0 4-4V4" ] []
        ]


cornerUpLeft : Html msg
cornerUpLeft =
    svgFeatherIcon "corner-up-left"
        [ Svg.polyline [ points "9 14 4 9 9 4" ] []
        , Svg.path [ d "M20 20v-7a4 4 0 0 0-4-4H4" ] []
        ]


cornerUpRight : Html msg
cornerUpRight =
    svgFeatherIcon "corner-up-right"
        [ Svg.polyline [ points "15 14 20 9 15 4" ] []
        , Svg.path [ d "M4 20v-7a4 4 0 0 1 4-4h12" ] []
        ]


cpu : Html msg
cpu =
    svgFeatherIcon "cpu"
        [ Svg.rect [ Svg.Attributes.x "4", y "4", width "16", height "16", rx "2", ry "2" ] []
        , Svg.rect [ Svg.Attributes.x "9", y "9", width "6", height "6" ] []
        , Svg.line [ x1 "9", y1 "1", x2 "9", y2 "4" ] []
        , Svg.line [ x1 "15", y1 "1", x2 "15", y2 "4" ] []
        , Svg.line [ x1 "9", y1 "20", x2 "9", y2 "23" ] []
        , Svg.line [ x1 "15", y1 "20", x2 "15", y2 "23" ] []
        , Svg.line [ x1 "20", y1 "9", x2 "23", y2 "9" ] []
        , Svg.line [ x1 "20", y1 "14", x2 "23", y2 "14" ] []
        , Svg.line [ x1 "1", y1 "9", x2 "4", y2 "9" ] []
        , Svg.line [ x1 "1", y1 "14", x2 "4", y2 "14" ] []
        ]


creditCard : Html msg
creditCard =
    svgFeatherIcon "credit-card"
        [ Svg.rect [ Svg.Attributes.x "1", y "4", width "22", height "16", rx "2", ry "2" ] []
        , Svg.line [ x1 "1", y1 "10", x2 "23", y2 "10" ] []
        ]


crop : Html msg
crop =
    svgFeatherIcon "crop"
        [ Svg.path [ d "M6.13 1L6 16a2 2 0 0 0 2 2h15" ] []
        , Svg.path [ d "M1 6.13L16 6a2 2 0 0 1 2 2v15" ] []
        ]


crosshair : Html msg
crosshair =
    svgFeatherIcon "crosshair"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "22", y1 "12", x2 "18", y2 "12" ] []
        , Svg.line [ x1 "6", y1 "12", x2 "2", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "6", x2 "12", y2 "2" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "18" ] []
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


disc : Html msg
disc =
    svgFeatherIcon "disc"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.circle [ cx "12", cy "12", r "3" ] []
        ]


dollarSign : Html msg
dollarSign =
    svgFeatherIcon "dollar-sign"
        [ Svg.line [ x1 "12", y1 "1", x2 "12", y2 "23" ] []
        , Svg.path [ d "M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6" ] []
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


droplet : Html msg
droplet =
    svgFeatherIcon "droplet"
        [ Svg.path [ d "M12 2.69l5.66 5.66a8 8 0 1 1-11.31 0z" ] []
        ]


edit : Html msg
edit =
    svgFeatherIcon "edit"
        [ Svg.path [ d "M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7" ] []
        , Svg.path [ d "M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z" ] []
        ]


edit2 : Html msg
edit2 =
    svgFeatherIcon "edit-2"
        [ Svg.path [ d "M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z" ] []
        ]


edit3 : Html msg
edit3 =
    svgFeatherIcon "edit-3"
        [ Svg.path [ d "M12 20h9" ] []
        , Svg.path [ d "M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z" ] []
        ]


externalLink : Html msg
externalLink =
    svgFeatherIcon "external-link"
        [ Svg.path [ d "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ] []
        , Svg.polyline [ points "15 3 21 3 21 9" ] []
        , Svg.line [ x1 "10", y1 "14", x2 "21", y2 "3" ] []
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


facebook : Html msg
facebook =
    svgFeatherIcon "facebook"
        [ Svg.path [ d "M18 2h-3a5 5 0 0 0-5 5v3H7v4h3v8h4v-8h3l1-4h-4V7a1 1 0 0 1 1-1h3z" ] []
        ]


fastForward : Html msg
fastForward =
    svgFeatherIcon "fast-forward"
        [ Svg.polygon [ points "13 19 22 12 13 5 13 19" ] []
        , Svg.polygon [ points "2 19 11 12 2 5 2 19" ] []
        ]


feather : Html msg
feather =
    svgFeatherIcon "feather"
        [ Svg.path [ d "M20.24 12.24a6 6 0 0 0-8.49-8.49L5 10.5V19h8.5z" ] []
        , Svg.line [ x1 "16", y1 "8", x2 "2", y2 "22" ] []
        , Svg.line [ x1 "17.5", y1 "15", x2 "9", y2 "15" ] []
        ]


figma : Html msg
figma =
    svgFeatherIcon "figma"
        [ Svg.path [ d "M5 5.5A3.5 3.5 0 0 1 8.5 2H12v7H8.5A3.5 3.5 0 0 1 5 5.5z" ] []
        , Svg.path [ d "M12 2h3.5a3.5 3.5 0 1 1 0 7H12V2z" ] []
        , Svg.path [ d "M12 12.5a3.5 3.5 0 1 1 7 0 3.5 3.5 0 1 1-7 0z" ] []
        , Svg.path [ d "M5 19.5A3.5 3.5 0 0 1 8.5 16H12v3.5a3.5 3.5 0 1 1-7 0z" ] []
        , Svg.path [ d "M5 12.5A3.5 3.5 0 0 1 8.5 9H12v7H8.5A3.5 3.5 0 0 1 5 12.5z" ] []
        ]


file : Html msg
file =
    svgFeatherIcon "file"
        [ Svg.path [ d "M13 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z" ] []
        , Svg.polyline [ points "13 2 13 9 20 9" ] []
        ]


fileMinus : Html msg
fileMinus =
    svgFeatherIcon "file-minus"
        [ Svg.path [ d "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" ] []
        , Svg.polyline [ points "14 2 14 8 20 8" ] []
        , Svg.line [ x1 "9", y1 "15", x2 "15", y2 "15" ] []
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


film : Html msg
film =
    svgFeatherIcon "film"
        [ Svg.rect [ Svg.Attributes.x "2", y "2", width "20", height "20", rx "2.18", ry "2.18" ] []
        , Svg.line [ x1 "7", y1 "2", x2 "7", y2 "22" ] []
        , Svg.line [ x1 "17", y1 "2", x2 "17", y2 "22" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "22", y2 "12" ] []
        , Svg.line [ x1 "2", y1 "7", x2 "7", y2 "7" ] []
        , Svg.line [ x1 "2", y1 "17", x2 "7", y2 "17" ] []
        , Svg.line [ x1 "17", y1 "17", x2 "22", y2 "17" ] []
        , Svg.line [ x1 "17", y1 "7", x2 "22", y2 "7" ] []
        ]


filter : Html msg
filter =
    svgFeatherIcon "filter"
        [ Svg.polygon [ points "22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3" ] []
        ]


flag : Html msg
flag =
    svgFeatherIcon "flag"
        [ Svg.path [ d "M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z" ] []
        , Svg.line [ x1 "4", y1 "22", x2 "4", y2 "15" ] []
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


framer : Html msg
framer =
    svgFeatherIcon "framer"
        [ Svg.path [ d "M5 16V9h14V2H5l14 14h-7m-7 0l7 7v-7m-7 0h7" ] []
        ]


frown : Html msg
frown =
    svgFeatherIcon "frown"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M16 16s-1.5-2-4-2-4 2-4 2" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "9.01", y2 "9" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "15.01", y2 "9" ] []
        ]


gift : Html msg
gift =
    svgFeatherIcon "gift"
        [ Svg.polyline [ points "20 12 20 22 4 22 4 12" ] []
        , Svg.rect [ Svg.Attributes.x "2", y "7", width "20", height "5" ] []
        , Svg.line [ x1 "12", y1 "22", x2 "12", y2 "7" ] []
        , Svg.path [ d "M12 7H7.5a2.5 2.5 0 0 1 0-5C11 2 12 7 12 7z" ] []
        , Svg.path [ d "M12 7h4.5a2.5 2.5 0 0 0 0-5C13 2 12 7 12 7z" ] []
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


gitlab : Html msg
gitlab =
    svgFeatherIcon "gitlab"
        [ Svg.path [ d "M22.65 14.39L12 22.13 1.35 14.39a.84.84 0 0 1-.3-.94l1.22-3.78 2.44-7.51A.42.42 0 0 1 4.82 2a.43.43 0 0 1 .58 0 .42.42 0 0 1 .11.18l2.44 7.49h8.1l2.44-7.51A.42.42 0 0 1 18.6 2a.43.43 0 0 1 .58 0 .42.42 0 0 1 .11.18l2.44 7.51L23 13.45a.84.84 0 0 1-.35.94z" ] []
        ]


globe : Html msg
globe =
    svgFeatherIcon "globe"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "22", y2 "12" ] []
        , Svg.path [ d "M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z" ] []
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


headphones : Html msg
headphones =
    svgFeatherIcon "headphones"
        [ Svg.path [ d "M3 18v-6a9 9 0 0 1 18 0v6" ] []
        , Svg.path [ d "M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1 2-2h3zM3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z" ] []
        ]


heart : Html msg
heart =
    svgFeatherIcon "heart"
        [ Svg.path [ d "M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z" ] []
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


inbox : Html msg
inbox =
    svgFeatherIcon "inbox"
        [ Svg.polyline [ points "22 12 16 12 14 15 10 15 8 12 2 12" ] []
        , Svg.path [ d "M5.45 5.11L2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z" ] []
        ]


info : Html msg
info =
    svgFeatherIcon "info"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "12", y1 "16", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "8" ] []
        ]


instagram : Html msg
instagram =
    svgFeatherIcon "instagram"
        [ Svg.rect [ Svg.Attributes.x "2", y "2", width "20", height "20", rx "5", ry "5" ] []
        , Svg.path [ d "M16 11.37A4 4 0 1 1 12.63 8 4 4 0 0 1 16 11.37z" ] []
        , Svg.line [ x1 "17.5", y1 "6.5", x2 "17.5", y2 "6.5" ] []
        ]


italic : Html msg
italic =
    svgFeatherIcon "italic"
        [ Svg.line [ x1 "19", y1 "4", x2 "10", y2 "4" ] []
        , Svg.line [ x1 "14", y1 "20", x2 "5", y2 "20" ] []
        , Svg.line [ x1 "15", y1 "4", x2 "9", y2 "20" ] []
        ]


key : Html msg
key =
    svgFeatherIcon "key"
        [ Svg.path [ d "M21 2l-2 2m-7.61 7.61a5.5 5.5 0 1 1-7.778 7.778 5.5 5.5 0 0 1 7.777-7.777zm0 0L15.5 7.5m0 0l3 3L22 7l-3-3m-3.5 3.5L19 4" ] []
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


lifeBuoy : Html msg
lifeBuoy =
    svgFeatherIcon "life-buoy"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.circle [ cx "12", cy "12", r "4" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "9.17", y2 "9.17" ] []
        , Svg.line [ x1 "14.83", y1 "14.83", x2 "19.07", y2 "19.07" ] []
        , Svg.line [ x1 "14.83", y1 "9.17", x2 "19.07", y2 "4.93" ] []
        , Svg.line [ x1 "14.83", y1 "9.17", x2 "18.36", y2 "5.64" ] []
        , Svg.line [ x1 "4.93", y1 "19.07", x2 "9.17", y2 "14.83" ] []
        ]


link : Html msg
link =
    svgFeatherIcon "link"
        [ Svg.path [ d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" ] []
        , Svg.path [ d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" ] []
        ]


link2 : Html msg
link2 =
    svgFeatherIcon "link-2"
        [ Svg.path [ d "M15 7h3a5 5 0 0 1 5 5 5 5 0 0 1-5 5h-3m-6 0H6a5 5 0 0 1-5-5 5 5 0 0 1 5-5h3" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


linkedin : Html msg
linkedin =
    svgFeatherIcon "linkedin"
        [ Svg.path [ d "M16 8a6 6 0 0 1 6 6v7h-4v-7a2 2 0 0 0-2-2 2 2 0 0 0-2 2v7h-4v-7a6 6 0 0 1 6-6z" ] []
        , Svg.rect [ Svg.Attributes.x "2", y "9", width "4", height "12" ] []
        , Svg.circle [ cx "4", cy "4", r "2" ] []
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


lock : Html msg
lock =
    svgFeatherIcon "lock"
        [ Svg.rect [ Svg.Attributes.x "3", y "11", width "18", height "11", rx "2", ry "2" ] []
        , Svg.path [ d "M7 11V7a5 5 0 0 1 10 0v4" ] []
        ]


logIn : Html msg
logIn =
    svgFeatherIcon "log-in"
        [ Svg.path [ d "M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4" ] []
        , Svg.polyline [ points "10 17 15 12 10 7" ] []
        , Svg.line [ x1 "15", y1 "12", x2 "3", y2 "12" ] []
        ]


logOut : Html msg
logOut =
    svgFeatherIcon "log-out"
        [ Svg.path [ d "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" ] []
        , Svg.polyline [ points "16 17 21 12 16 7" ] []
        , Svg.line [ x1 "21", y1 "12", x2 "9", y2 "12" ] []
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


maximize : Html msg
maximize =
    svgFeatherIcon "maximize"
        [ Svg.path [ d "M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3" ] []
        ]


maximize2 : Html msg
maximize2 =
    svgFeatherIcon "maximize-2"
        [ Svg.polyline [ points "15 3 21 3 21 9" ] []
        , Svg.polyline [ points "9 21 3 21 3 15" ] []
        , Svg.line [ x1 "21", y1 "3", x2 "14", y2 "10" ] []
        , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
        ]


meh : Html msg
meh =
    svgFeatherIcon "meh"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "8", y1 "15", x2 "16", y2 "15" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "9.01", y2 "9" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "15.01", y2 "9" ] []
        ]


menu : Html msg
menu =
    svgFeatherIcon "menu"
        [ Svg.line [ x1 "3", y1 "12", x2 "21", y2 "12" ] []
        , Svg.line [ x1 "3", y1 "6", x2 "21", y2 "6" ] []
        , Svg.line [ x1 "3", y1 "18", x2 "21", y2 "18" ] []
        ]


messageCircle : Html msg
messageCircle =
    svgFeatherIcon "message-circle"
        [ Svg.path [ d "M21 11.5a8.38 8.38 0 0 1-.9 3.8 8.5 8.5 0 0 1-7.6 4.7 8.38 8.38 0 0 1-3.8-.9L3 21l1.9-5.7a8.38 8.38 0 0 1-.9-3.8 8.5 8.5 0 0 1 4.7-7.6 8.38 8.38 0 0 1 3.8-.9h.5a8.48 8.48 0 0 1 8 8v.5z" ] []
        ]


messageSquare : Html msg
messageSquare =
    svgFeatherIcon "message-square"
        [ Svg.path [ d "M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z" ] []
        ]


mic : Html msg
mic =
    svgFeatherIcon "mic"
        [ Svg.path [ d "M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z" ] []
        , Svg.path [ d "M19 10v2a7 7 0 0 1-14 0v-2" ] []
        , Svg.line [ x1 "12", y1 "19", x2 "12", y2 "23" ] []
        , Svg.line [ x1 "8", y1 "23", x2 "16", y2 "23" ] []
        ]


micOff : Html msg
micOff =
    svgFeatherIcon "mic-off"
        [ Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        , Svg.path [ d "M9 9v3a3 3 0 0 0 5.12 2.12M15 9.34V4a3 3 0 0 0-5.94-.6" ] []
        , Svg.path [ d "M17 16.95A7 7 0 0 1 5 12v-2m14 0v2a7 7 0 0 1-.11 1.23" ] []
        , Svg.line [ x1 "12", y1 "19", x2 "12", y2 "23" ] []
        , Svg.line [ x1 "8", y1 "23", x2 "16", y2 "23" ] []
        ]


minimize : Html msg
minimize =
    svgFeatherIcon "minimize"
        [ Svg.path [ d "M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3" ] []
        ]


minimize2 : Html msg
minimize2 =
    svgFeatherIcon "minimize-2"
        [ Svg.polyline [ points "4 14 10 14 10 20" ] []
        , Svg.polyline [ points "20 10 14 10 14 4" ] []
        , Svg.line [ x1 "14", y1 "10", x2 "21", y2 "3" ] []
        , Svg.line [ x1 "3", y1 "21", x2 "10", y2 "14" ] []
        ]


minus : Html msg
minus =
    svgFeatherIcon "minus"
        [ Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


minusCircle : Html msg
minusCircle =
    svgFeatherIcon "minus-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


minusSquare : Html msg
minusSquare =
    svgFeatherIcon "minus-square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "16", y2 "12" ] []
        ]


monitor : Html msg
monitor =
    svgFeatherIcon "monitor"
        [ Svg.rect [ Svg.Attributes.x "2", y "3", width "20", height "14", rx "2", ry "2" ] []
        , Svg.line [ x1 "8", y1 "21", x2 "16", y2 "21" ] []
        , Svg.line [ x1 "12", y1 "17", x2 "12", y2 "21" ] []
        ]


moon : Html msg
moon =
    svgFeatherIcon "moon"
        [ Svg.path [ d "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z" ] []
        ]


moreHorizontal : Html msg
moreHorizontal =
    svgFeatherIcon "more-horizontal"
        [ Svg.circle [ cx "12", cy "12", r "1" ] []
        , Svg.circle [ cx "19", cy "12", r "1" ] []
        , Svg.circle [ cx "5", cy "12", r "1" ] []
        ]


moreVertical : Html msg
moreVertical =
    svgFeatherIcon "more-vertical"
        [ Svg.circle [ cx "12", cy "12", r "1" ] []
        , Svg.circle [ cx "12", cy "5", r "1" ] []
        , Svg.circle [ cx "12", cy "19", r "1" ] []
        ]


mousePointer : Html msg
mousePointer =
    svgFeatherIcon "mouse-pointer"
        [ Svg.path [ d "M3 3l7.07 16.97 2.51-7.39 7.39-2.51L3 3z" ] []
        , Svg.path [ d "M13 13l6 6" ] []
        ]


move : Html msg
move =
    svgFeatherIcon "move"
        [ Svg.polyline [ points "5 9 2 12 5 15" ] []
        , Svg.polyline [ points "9 5 12 2 15 5" ] []
        , Svg.polyline [ points "15 19 12 22 9 19" ] []
        , Svg.polyline [ points "19 9 22 12 19 15" ] []
        , Svg.line [ x1 "2", y1 "12", x2 "22", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "22" ] []
        ]


music : Html msg
music =
    svgFeatherIcon "music"
        [ Svg.path [ d "M9 18V5l12-2v13" ] []
        , Svg.circle [ cx "6", cy "18", r "3" ] []
        , Svg.circle [ cx "18", cy "16", r "3" ] []
        ]


navigation : Html msg
navigation =
    svgFeatherIcon "navigation"
        [ Svg.polygon [ points "3 11 22 2 13 21 11 13 3 11" ] []
        ]


navigation2 : Html msg
navigation2 =
    svgFeatherIcon "navigation-2"
        [ Svg.polygon [ points "12 2 19 21 12 17 5 21 12 2" ] []
        ]


octagon : Html msg
octagon =
    svgFeatherIcon "octagon"
        [ Svg.polygon [ points "7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2" ] []
        ]


package : Html msg
package =
    svgFeatherIcon "package"
        [ Svg.line [ x1 "16.5", y1 "9.4", x2 "7.5", y2 "4.21" ] []
        , Svg.path [ d "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z" ] []
        , Svg.polyline [ points "3.27 6.96 12 12.01 20.73 6.96" ] []
        , Svg.line [ x1 "12", y1 "22.08", x2 "12", y2 "12" ] []
        ]


paperclip : Html msg
paperclip =
    svgFeatherIcon "paperclip"
        [ Svg.path [ d "M21.44 11.05l-9.19 9.19a6 6 0 0 1-8.49-8.49l9.19-9.19a4 4 0 0 1 5.66 5.66l-9.2 9.19a2 2 0 0 1-2.83-2.83l8.49-8.48" ] []
        ]


pause : Html msg
pause =
    svgFeatherIcon "pause"
        [ Svg.rect [ Svg.Attributes.x "6", y "4", width "4", height "16" ] []
        , Svg.rect [ Svg.Attributes.x "14", y "4", width "4", height "16" ] []
        ]


pauseCircle : Html msg
pauseCircle =
    svgFeatherIcon "pause-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "10", y1 "15", x2 "10", y2 "9" ] []
        , Svg.line [ x1 "14", y1 "15", x2 "14", y2 "9" ] []
        ]


penTool : Html msg
penTool =
    svgFeatherIcon "pen-tool"
        [ Svg.path [ d "M12 19l7-7 3 3-7 7-3-3z" ] []
        , Svg.path [ d "M18 13l-1.5-7.5L2 2l3.5 14.5L13 18l5-5z" ] []
        , Svg.path [ d "M2 2l7.586 7.586" ] []
        , Svg.circle [ cx "11", cy "11", r "2" ] []
        ]


percent : Html msg
percent =
    svgFeatherIcon "percent"
        [ Svg.line [ x1 "19", y1 "5", x2 "5", y2 "19" ] []
        , Svg.circle [ cx "6.5", cy "6.5", r "2.5" ] []
        , Svg.circle [ cx "17.5", cy "17.5", r "2.5" ] []
        ]


phone : Html msg
phone =
    svgFeatherIcon "phone"
        [ Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneCall : Html msg
phoneCall =
    svgFeatherIcon "phone-call"
        [ Svg.path [ d "M15.05 5A5 5 0 0 1 19 8.95M15.05 1A9 9 0 0 1 23 8.94m-1 7.98v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneForwarded : Html msg
phoneForwarded =
    svgFeatherIcon "phone-forwarded"
        [ Svg.polyline [ points "19 1 23 5 19 9" ] []
        , Svg.line [ x1 "15", y1 "5", x2 "23", y2 "5" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneIncoming : Html msg
phoneIncoming =
    svgFeatherIcon "phone-incoming"
        [ Svg.polyline [ points "16 2 16 8 22 8" ] []
        , Svg.line [ x1 "23", y1 "1", x2 "16", y2 "8" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneMissed : Html msg
phoneMissed =
    svgFeatherIcon "phone-missed"
        [ Svg.line [ x1 "23", y1 "1", x2 "17", y2 "7" ] []
        , Svg.line [ x1 "17", y1 "1", x2 "23", y2 "7" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


phoneOff : Html msg
phoneOff =
    svgFeatherIcon "phone-off"
        [ Svg.path [ d "M10.68 13.31a16 16 0 0 0 3.41 2.6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7 2 2 0 0 1 1.72 2v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.42 19.42 0 0 1-3.33-2.67m-2.67-3.34a19.79 19.79 0 0 1-3.07-8.63A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91" ] []
        , Svg.line [ x1 "23", y1 "1", x2 "1", y2 "23" ] []
        ]


phoneOutgoing : Html msg
phoneOutgoing =
    svgFeatherIcon "phone-outgoing"
        [ Svg.polyline [ points "23 7 23 1 17 1" ] []
        , Svg.line [ x1 "16", y1 "8", x2 "23", y2 "1" ] []
        , Svg.path [ d "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7A2 2 0 0 1 22 16.92z" ] []
        ]


pieChart : Html msg
pieChart =
    svgFeatherIcon "pie-chart"
        [ Svg.path [ d "M21.21 15.89A10 10 0 1 1 8 2.83" ] []
        , Svg.path [ d "M22 12A10 10 0 0 0 12 2v10z" ] []
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


plus : Html msg
plus =
    svgFeatherIcon "plus"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
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


pocket : Html msg
pocket =
    svgFeatherIcon "pocket"
        [ Svg.path [ d "M4 3h16a2 2 0 0 1 2 2v6a10 10 0 0 1-10 10A10 10 0 0 1 2 11V5a2 2 0 0 1 2-2z" ] []
        , Svg.polyline [ points "8 10 12 14 16 10" ] []
        ]


power : Html msg
power =
    svgFeatherIcon "power"
        [ Svg.path [ d "M18.36 6.64a9 9 0 1 1-12.73 0" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "12" ] []
        ]


printer : Html msg
printer =
    svgFeatherIcon "printer"
        [ Svg.polyline [ points "6 9 6 2 18 2 18 9" ] []
        , Svg.path [ d "M6 18H4a2 2 0 0 1-2-2v-5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2h-2" ] []
        , Svg.rect [ Svg.Attributes.x "6", y "14", width "12", height "8" ] []
        ]


radio : Html msg
radio =
    svgFeatherIcon "radio"
        [ Svg.circle [ cx "12", cy "12", r "2" ] []
        , Svg.path [ d "M16.24 7.76a6 6 0 0 1 0 8.49m-8.48-.01a6 6 0 0 1 0-8.49m11.31-2.82a10 10 0 0 1 0 14.14m-14.14 0a10 10 0 0 1 0-14.14" ] []
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


rewind : Html msg
rewind =
    svgFeatherIcon "rewind"
        [ Svg.polygon [ points "11 19 2 12 11 5 11 19" ] []
        , Svg.polygon [ points "22 19 13 12 22 5 22 19" ] []
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


scissors : Html msg
scissors =
    svgFeatherIcon "scissors"
        [ Svg.circle [ cx "6", cy "6", r "3" ] []
        , Svg.circle [ cx "6", cy "18", r "3" ] []
        , Svg.line [ x1 "20", y1 "4", x2 "8.12", y2 "15.88" ] []
        , Svg.line [ x1 "14.47", y1 "14.48", x2 "20", y2 "20" ] []
        , Svg.line [ x1 "8.12", y1 "8.12", x2 "12", y2 "12" ] []
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


shield : Html msg
shield =
    svgFeatherIcon "shield"
        [ Svg.path [ d "M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z" ] []
        ]


shieldOff : Html msg
shieldOff =
    svgFeatherIcon "shield-off"
        [ Svg.path [ d "M19.69 14a6.9 6.9 0 0 0 .31-2V5l-8-3-3.16 1.18" ] []
        , Svg.path [ d "M4.73 4.73L4 5v7c0 6 8 10 8 10a20.29 20.29 0 0 0 5.62-4.38" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


shoppingBag : Html msg
shoppingBag =
    svgFeatherIcon "shopping-bag"
        [ Svg.path [ d "M6 2L3 6v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2V6l-3-4z" ] []
        , Svg.line [ x1 "3", y1 "6", x2 "21", y2 "6" ] []
        , Svg.path [ d "M16 10a4 4 0 0 1-8 0" ] []
        ]


shoppingCart : Html msg
shoppingCart =
    svgFeatherIcon "shopping-cart"
        [ Svg.circle [ cx "9", cy "21", r "1" ] []
        , Svg.circle [ cx "20", cy "21", r "1" ] []
        , Svg.path [ d "M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6" ] []
        ]


shuffle : Html msg
shuffle =
    svgFeatherIcon "shuffle"
        [ Svg.polyline [ points "16 3 21 3 21 8" ] []
        , Svg.line [ x1 "4", y1 "20", x2 "21", y2 "3" ] []
        , Svg.polyline [ points "21 16 21 21 16 21" ] []
        , Svg.line [ x1 "15", y1 "15", x2 "21", y2 "21" ] []
        , Svg.line [ x1 "4", y1 "4", x2 "9", y2 "9" ] []
        ]


sidebar : Html msg
sidebar =
    svgFeatherIcon "sidebar"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "9", y1 "3", x2 "9", y2 "21" ] []
        ]


skipBack : Html msg
skipBack =
    svgFeatherIcon "skip-back"
        [ Svg.polygon [ points "19 20 9 12 19 4 19 20" ] []
        , Svg.line [ x1 "5", y1 "19", x2 "5", y2 "5" ] []
        ]


skipForward : Html msg
skipForward =
    svgFeatherIcon "skip-forward"
        [ Svg.polygon [ points "5 4 15 12 5 20 5 4" ] []
        , Svg.line [ x1 "19", y1 "5", x2 "19", y2 "19" ] []
        ]


slack : Html msg
slack =
    svgFeatherIcon "slack"
        [ Svg.path [ d "M14.5 10c-.83 0-1.5-.67-1.5-1.5v-5c0-.83.67-1.5 1.5-1.5s1.5.67 1.5 1.5v5c0 .83-.67 1.5-1.5 1.5z" ] []
        , Svg.path [ d "M20.5 10H19V8.5c0-.83.67-1.5 1.5-1.5s1.5.67 1.5 1.5-.67 1.5-1.5 1.5z" ] []
        , Svg.path [ d "M9.5 14c.83 0 1.5.67 1.5 1.5v5c0 .83-.67 1.5-1.5 1.5S8 21.33 8 20.5v-5c0-.83.67-1.5 1.5-1.5z" ] []
        , Svg.path [ d "M3.5 14H5v1.5c0 .83-.67 1.5-1.5 1.5S2 16.33 2 15.5 2.67 14 3.5 14z" ] []
        , Svg.path [ d "M14 14.5c0-.83.67-1.5 1.5-1.5h5c.83 0 1.5.67 1.5 1.5s-.67 1.5-1.5 1.5h-5c-.83 0-1.5-.67-1.5-1.5z" ] []
        , Svg.path [ d "M15.5 19H14v1.5c0 .83.67 1.5 1.5 1.5s1.5-.67 1.5-1.5-.67-1.5-1.5-1.5z" ] []
        , Svg.path [ d "M10 9.5C10 8.67 9.33 8 8.5 8h-5C2.67 8 2 8.67 2 9.5S2.67 11 3.5 11h5c.83 0 1.5-.67 1.5-1.5z" ] []
        , Svg.path [ d "M8.5 5H10V3.5C10 2.67 9.33 2 8.5 2S7 2.67 7 3.5 7.67 5 8.5 5z" ] []
        ]


slash : Html msg
slash =
    svgFeatherIcon "slash"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "4.93", y1 "4.93", x2 "19.07", y2 "19.07" ] []
        ]


sliders : Html msg
sliders =
    svgFeatherIcon "sliders"
        [ Svg.line [ x1 "4", y1 "21", x2 "4", y2 "14" ] []
        , Svg.line [ x1 "4", y1 "10", x2 "4", y2 "3" ] []
        , Svg.line [ x1 "12", y1 "21", x2 "12", y2 "12" ] []
        , Svg.line [ x1 "12", y1 "8", x2 "12", y2 "3" ] []
        , Svg.line [ x1 "20", y1 "21", x2 "20", y2 "16" ] []
        , Svg.line [ x1 "20", y1 "12", x2 "20", y2 "3" ] []
        , Svg.line [ x1 "1", y1 "14", x2 "7", y2 "14" ] []
        , Svg.line [ x1 "9", y1 "8", x2 "15", y2 "8" ] []
        , Svg.line [ x1 "17", y1 "16", x2 "23", y2 "16" ] []
        ]


smartphone : Html msg
smartphone =
    svgFeatherIcon "smartphone"
        [ Svg.rect [ Svg.Attributes.x "5", y "2", width "14", height "20", rx "2", ry "2" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "18" ] []
        ]


smile : Html msg
smile =
    svgFeatherIcon "smile"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.path [ d "M8 14s1.5 2 4 2 4-2 4-2" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "9.01", y2 "9" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "15.01", y2 "9" ] []
        ]


speaker : Html msg
speaker =
    svgFeatherIcon "speaker"
        [ Svg.rect [ Svg.Attributes.x "4", y "2", width "16", height "20", rx "2", ry "2" ] []
        , Svg.circle [ cx "12", cy "14", r "4" ] []
        , Svg.line [ x1 "12", y1 "6", x2 "12", y2 "6" ] []
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


stopCircle : Html msg
stopCircle =
    svgFeatherIcon "stop-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.rect [ Svg.Attributes.x "9", y "9", width "6", height "6" ] []
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


sunrise : Html msg
sunrise =
    svgFeatherIcon "sunrise"
        [ Svg.path [ d "M17 18a5 5 0 0 0-10 0" ] []
        , Svg.line [ x1 "12", y1 "2", x2 "12", y2 "9" ] []
        , Svg.line [ x1 "4.22", y1 "10.22", x2 "5.64", y2 "11.64" ] []
        , Svg.line [ x1 "1", y1 "18", x2 "3", y2 "18" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "23", y2 "18" ] []
        , Svg.line [ x1 "18.36", y1 "11.64", x2 "19.78", y2 "10.22" ] []
        , Svg.line [ x1 "23", y1 "22", x2 "1", y2 "22" ] []
        , Svg.polyline [ points "8 6 12 2 16 6" ] []
        ]


sunset : Html msg
sunset =
    svgFeatherIcon "sunset"
        [ Svg.path [ d "M17 18a5 5 0 0 0-10 0" ] []
        , Svg.line [ x1 "12", y1 "9", x2 "12", y2 "2" ] []
        , Svg.line [ x1 "4.22", y1 "10.22", x2 "5.64", y2 "11.64" ] []
        , Svg.line [ x1 "1", y1 "18", x2 "3", y2 "18" ] []
        , Svg.line [ x1 "21", y1 "18", x2 "23", y2 "18" ] []
        , Svg.line [ x1 "18.36", y1 "11.64", x2 "19.78", y2 "10.22" ] []
        , Svg.line [ x1 "23", y1 "22", x2 "1", y2 "22" ] []
        , Svg.polyline [ points "16 5 12 9 8 5" ] []
        ]


tablet : Html msg
tablet =
    svgFeatherIcon "tablet"
        [ Svg.rect [ Svg.Attributes.x "4", y "2", width "16", height "20", rx "2", ry "2", transform "rotate(180 12 12)" ] []
        , Svg.line [ x1 "12", y1 "18", x2 "12", y2 "18" ] []
        ]


tag : Html msg
tag =
    svgFeatherIcon "tag"
        [ Svg.path [ d "M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z" ] []
        , Svg.line [ x1 "7", y1 "7", x2 "7", y2 "7" ] []
        ]


target : Html msg
target =
    svgFeatherIcon "target"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.circle [ cx "12", cy "12", r "6" ] []
        , Svg.circle [ cx "12", cy "12", r "2" ] []
        ]


terminal : Html msg
terminal =
    svgFeatherIcon "terminal"
        [ Svg.polyline [ points "4 17 10 11 4 5" ] []
        , Svg.line [ x1 "12", y1 "19", x2 "20", y2 "19" ] []
        ]


thermometer : Html msg
thermometer =
    svgFeatherIcon "thermometer"
        [ Svg.path [ d "M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z" ] []
        ]


thumbsDown : Html msg
thumbsDown =
    svgFeatherIcon "thumbs-down"
        [ Svg.path [ d "M10 15v4a3 3 0 0 0 3 3l4-9V2H5.72a2 2 0 0 0-2 1.7l-1.38 9a2 2 0 0 0 2 2.3zm7-13h2.67A2.31 2.31 0 0 1 22 4v7a2.31 2.31 0 0 1-2.33 2H17" ] []
        ]


thumbsUp : Html msg
thumbsUp =
    svgFeatherIcon "thumbs-up"
        [ Svg.path [ d "M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3zM7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3" ] []
        ]


tool : Html msg
tool =
    svgFeatherIcon "tool"
        [ Svg.path [ d "M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 7.94-7.94l-3.76 3.76z" ] []
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


trash : Html msg
trash =
    svgFeatherIcon "trash"
        [ Svg.polyline [ points "3 6 5 6 21 6" ] []
        , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
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


trendingDown : Html msg
trendingDown =
    svgFeatherIcon "trending-down"
        [ Svg.polyline [ points "23 18 13.5 8.5 8.5 13.5 1 6" ] []
        , Svg.polyline [ points "17 18 23 18 23 12" ] []
        ]


trendingUp : Html msg
trendingUp =
    svgFeatherIcon "trending-up"
        [ Svg.polyline [ points "23 6 13.5 15.5 8.5 10.5 1 18" ] []
        , Svg.polyline [ points "17 6 23 6 23 12" ] []
        ]


triangle : Html msg
triangle =
    svgFeatherIcon "triangle"
        [ Svg.path [ d "M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z" ] []
        ]


truck : Html msg
truck =
    svgFeatherIcon "truck"
        [ Svg.rect [ Svg.Attributes.x "1", y "3", width "15", height "13" ] []
        , Svg.polygon [ points "16 8 20 8 23 11 23 16 16 16 16 8" ] []
        , Svg.circle [ cx "5.5", cy "18.5", r "2.5" ] []
        , Svg.circle [ cx "18.5", cy "18.5", r "2.5" ] []
        ]


tv : Html msg
tv =
    svgFeatherIcon "tv"
        [ Svg.rect [ Svg.Attributes.x "2", y "7", width "20", height "15", rx "2", ry "2" ] []
        , Svg.polyline [ points "17 2 12 7 7 2" ] []
        ]


twitter : Html msg
twitter =
    svgFeatherIcon "twitter"
        [ Svg.path [ d "M23 3a10.9 10.9 0 0 1-3.14 1.53 4.48 4.48 0 0 0-7.86 3v1A10.66 10.66 0 0 1 3 4s-4 9 5 13a11.64 11.64 0 0 1-7 2c9 5 20 0 20-11.5a4.5 4.5 0 0 0-.08-.83A7.72 7.72 0 0 0 23 3z" ] []
        ]


type_ : Html msg
type_ =
    svgFeatherIcon "type"
        [ Svg.polyline [ points "4 7 4 4 20 4 20 7" ] []
        , Svg.line [ x1 "9", y1 "20", x2 "15", y2 "20" ] []
        , Svg.line [ x1 "12", y1 "4", x2 "12", y2 "20" ] []
        ]


umbrella : Html msg
umbrella =
    svgFeatherIcon "umbrella"
        [ Svg.path [ d "M23 12a11.05 11.05 0 0 0-22 0zm-5 7a3 3 0 0 1-6 0v-7" ] []
        ]


underline : Html msg
underline =
    svgFeatherIcon "underline"
        [ Svg.path [ d "M6 3v7a6 6 0 0 0 6 6 6 6 0 0 0 6-6V3" ] []
        , Svg.line [ x1 "4", y1 "21", x2 "20", y2 "21" ] []
        ]


unlock : Html msg
unlock =
    svgFeatherIcon "unlock"
        [ Svg.rect [ Svg.Attributes.x "3", y "11", width "18", height "11", rx "2", ry "2" ] []
        , Svg.path [ d "M7 11V7a5 5 0 0 1 9.9-1" ] []
        ]


upload : Html msg
upload =
    svgFeatherIcon "upload"
        [ Svg.path [ d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" ] []
        , Svg.polyline [ points "17 8 12 3 7 8" ] []
        , Svg.line [ x1 "12", y1 "3", x2 "12", y2 "15" ] []
        ]


uploadCloud : Html msg
uploadCloud =
    svgFeatherIcon "upload-cloud"
        [ Svg.polyline [ points "16 16 12 12 8 16" ] []
        , Svg.line [ x1 "12", y1 "12", x2 "12", y2 "21" ] []
        , Svg.path [ d "M20.39 18.39A5 5 0 0 0 18 9h-1.26A8 8 0 1 0 3 16.3" ] []
        , Svg.polyline [ points "16 16 12 12 8 16" ] []
        ]


user : Html msg
user =
    svgFeatherIcon "user"
        [ Svg.path [ d "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "12", cy "7", r "4" ] []
        ]


userCheck : Html msg
userCheck =
    svgFeatherIcon "user-check"
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "8.5", cy "7", r "4" ] []
        , Svg.polyline [ points "17 11 19 13 23 9" ] []
        ]


userMinus : Html msg
userMinus =
    svgFeatherIcon "user-minus"
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "8.5", cy "7", r "4" ] []
        , Svg.line [ x1 "23", y1 "11", x2 "17", y2 "11" ] []
        ]


userPlus : Html msg
userPlus =
    svgFeatherIcon "user-plus"
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "8.5", cy "7", r "4" ] []
        , Svg.line [ x1 "20", y1 "8", x2 "20", y2 "14" ] []
        , Svg.line [ x1 "23", y1 "11", x2 "17", y2 "11" ] []
        ]


userX : Html msg
userX =
    svgFeatherIcon "user-x"
        [ Svg.path [ d "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ cx "8.5", cy "7", r "4" ] []
        , Svg.line [ x1 "18", y1 "8", x2 "23", y2 "13" ] []
        , Svg.line [ x1 "23", y1 "8", x2 "18", y2 "13" ] []
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


voicemail : Html msg
voicemail =
    svgFeatherIcon "voicemail"
        [ Svg.circle [ cx "5.5", cy "11.5", r "4.5" ] []
        , Svg.circle [ cx "18.5", cy "11.5", r "4.5" ] []
        , Svg.line [ x1 "5.5", y1 "16", x2 "18.5", y2 "16" ] []
        ]


volume : Html msg
volume =
    svgFeatherIcon "volume"
        [ Svg.polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        ]


volume1 : Html msg
volume1 =
    svgFeatherIcon "volume-1"
        [ Svg.polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , Svg.path [ d "M15.54 8.46a5 5 0 0 1 0 7.07" ] []
        ]


volume2 : Html msg
volume2 =
    svgFeatherIcon "volume-2"
        [ Svg.polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , Svg.path [ d "M19.07 4.93a10 10 0 0 1 0 14.14M15.54 8.46a5 5 0 0 1 0 7.07" ] []
        ]


volumeX : Html msg
volumeX =
    svgFeatherIcon "volume-x"
        [ Svg.polygon [ points "11 5 6 9 2 9 2 15 6 15 11 19 11 5" ] []
        , Svg.line [ x1 "23", y1 "9", x2 "17", y2 "15" ] []
        , Svg.line [ x1 "17", y1 "9", x2 "23", y2 "15" ] []
        ]


watch : Html msg
watch =
    svgFeatherIcon "watch"
        [ Svg.circle [ cx "12", cy "12", r "7" ] []
        , Svg.polyline [ points "12 9 12 12 13.5 13.5" ] []
        , Svg.path [ d "M16.51 17.35l-.35 3.83a2 2 0 0 1-2 1.82H9.83a2 2 0 0 1-2-1.82l-.35-3.83m.01-10.7l.35-3.83A2 2 0 0 1 9.83 1h4.35a2 2 0 0 1 2 1.82l.35 3.83" ] []
        ]


wifi : Html msg
wifi =
    svgFeatherIcon "wifi"
        [ Svg.path [ d "M5 12.55a11 11 0 0 1 14.08 0" ] []
        , Svg.path [ d "M1.42 9a16 16 0 0 1 21.16 0" ] []
        , Svg.path [ d "M8.53 16.11a6 6 0 0 1 6.95 0" ] []
        , Svg.line [ x1 "12", y1 "20", x2 "12", y2 "20" ] []
        ]


wifiOff : Html msg
wifiOff =
    svgFeatherIcon "wifi-off"
        [ Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        , Svg.path [ d "M16.72 11.06A10.94 10.94 0 0 1 19 12.55" ] []
        , Svg.path [ d "M5 12.55a10.94 10.94 0 0 1 5.17-2.39" ] []
        , Svg.path [ d "M10.71 5.05A16 16 0 0 1 22.58 9" ] []
        , Svg.path [ d "M1.42 9a15.91 15.91 0 0 1 4.7-2.88" ] []
        , Svg.path [ d "M8.53 16.11a6 6 0 0 1 6.95 0" ] []
        , Svg.line [ x1 "12", y1 "20", x2 "12", y2 "20" ] []
        ]


wind : Html msg
wind =
    svgFeatherIcon "wind"
        [ Svg.path [ d "M9.59 4.59A2 2 0 1 1 11 8H2m10.59 11.41A2 2 0 1 0 14 16H2m15.73-8.27A2.5 2.5 0 1 1 19.5 12H2" ] []
        ]


x : Html msg
x =
    svgFeatherIcon "x"
        [ Svg.line [ x1 "18", y1 "6", x2 "6", y2 "18" ] []
        , Svg.line [ x1 "6", y1 "6", x2 "18", y2 "18" ] []
        ]


xCircle : Html msg
xCircle =
    svgFeatherIcon "x-circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        ]


xOctagon : Html msg
xOctagon =
    svgFeatherIcon "x-octagon"
        [ Svg.polygon [ points "7.86 2 16.14 2 22 7.86 22 16.14 16.14 22 7.86 22 2 16.14 2 7.86 7.86 2" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        ]


xSquare : Html msg
xSquare =
    svgFeatherIcon "x-square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        ]


youtube : Html msg
youtube =
    svgFeatherIcon "youtube"
        [ Svg.path [ d "M22.54 6.42a2.78 2.78 0 0 0-1.94-2C18.88 4 12 4 12 4s-6.88 0-8.6.46a2.78 2.78 0 0 0-1.94 2A29 29 0 0 0 1 11.75a29 29 0 0 0 .46 5.33A2.78 2.78 0 0 0 3.4 19c1.72.46 8.6.46 8.6.46s6.88 0 8.6-.46a2.78 2.78 0 0 0 1.94-2 29 29 0 0 0 .46-5.25 29 29 0 0 0-.46-5.33z" ] []
        , Svg.polygon [ points "9.75 15.02 15.5 11.75 9.75 8.48 9.75 15.02" ] []
        ]


zap : Html msg
zap =
    svgFeatherIcon "zap"
        [ Svg.polygon [ points "13 2 3 14 12 14 11 22 21 10 12 10 13 2" ] []
        ]


zapOff : Html msg
zapOff =
    svgFeatherIcon "zap-off"
        [ Svg.polyline [ points "12.41 6.75 13 2 10.57 4.92" ] []
        , Svg.polyline [ points "18.57 12.91 21 10 15.66 10" ] []
        , Svg.polyline [ points "8 8 3 14 12 14 11 22 16 16" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
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
