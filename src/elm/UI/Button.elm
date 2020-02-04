module UI.Button exposing (add, addFile, addNode, addTextFile, button, delete, diffTree, download, next, refresh, renderDAGasTable, renderDAGasTree, save)

import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import UI.Colors as Colors exposing (..)
import UI.Icons as Icons


type Button msg
    = Button Options msg


type alias Options =
    { role : Role
    , disabled : Bool
    , text : String
    , icon : Icon
    , size : Size
    , counter : Int
    , title : Maybe String
    }


type Role
    = Success
    | Danger
    | Info
    | Blank


type Icon
    = Save
    | Download
    | Edit
    | Show
    | Hide
    | Add
    | Delete
    | Refresh
    | None
    | AddFile
    | AddTextFile
    | PullRequest
    | Grid
    | ChevronUp


type Size
    = Small
    | Medium
    | Big


defaultOptions =
    { role = Info
    , disabled = False
    , icon = None
    , text = ""
    , size = Medium
    , counter = 0
    , title = Nothing
    }


save : Bool -> Int -> msg -> Element msg
save prediction int msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon Save
        |> withSize Medium
        |> withCounter int
        |> withTitle "Сохранить хранилище в IPFS"
        |> disable prediction
        |> renderButton


addFile : msg -> Element msg
addFile msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon AddFile
        |> withSize Medium
        |> withTitle "Добавить файл"
        |> renderButton


addTextFile : msg -> Element msg
addTextFile msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon AddTextFile
        |> withSize Medium
        |> withTitle "Добавить текст"
        |> renderButton


refresh : msg -> Element msg
refresh msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon Refresh
        |> withSize Small
        |> renderButton


delete : Bool -> msg -> Element msg
delete prediction msg =
    Button defaultOptions msg
        |> withRole Danger
        |> withSize Medium
        |> withIcon Delete
        |> withTitle "Удалить текущую выделенную ячейку и все дочерние ячейки"
        |> disable prediction
        |> renderButton


add : msg -> Element msg
add msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon Add
        |> withText "Добавить"
        |> renderButton


addNode : msg -> Element msg
addNode msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon Add
        |> withTitle "Добавить дочернюю ячейку"
        |> renderButton


diffTree : msg -> Element msg
diffTree msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon PullRequest
        |> withTitle "Сравнить деревья"
        |> renderButton


renderDAGasTree : msg -> Element msg
renderDAGasTree msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon ChevronUp
        |> withTitle "Показать в виде дерева"
        |> renderButton


renderDAGasTable : msg -> Element msg
renderDAGasTable msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon Grid
        |> withTitle "Показать в виде таблицы"
        |> renderButton


download : msg -> Element msg
download msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon Download
        |> withTitle "Сохранить локально весь репозиторий"
        |> disable False
        |> renderButton


next : msg -> Element msg
next msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Small
        |> withText "Дальше..."
        |> renderButton


withSize : Size -> Button msg -> Button msg
withSize size (Button options msg) =
    Button { options | size = size } msg


withText : String -> Button msg -> Button msg
withText text (Button options msg) =
    Button { options | text = text } msg


withIcon : Icon -> Button msg -> Button msg
withIcon icon (Button options msg) =
    Button { options | icon = icon } msg


withTitle : String -> Button msg -> Button msg
withTitle title (Button options msg) =
    Button { options | title = Just title } msg


withRole : Role -> Button msg -> Button msg
withRole role (Button options msg) =
    Button { options | role = role } msg


withCounter : Int -> Button msg -> Button msg
withCounter int (Button options msg) =
    Button { options | counter = int } msg


disable : Bool -> Button msg -> Button msg
disable bool (Button options msg) =
    Button { options | disabled = bool } msg


renderButton : Button msg -> Element msg
renderButton (Button options msg) =
    let
        counterIsZero =
            options.counter == 0
    in
    Input.button
        [ padding 5
        , alignLeft
        , Border.rounded 5
        , mouseOver <| [ Background.color <| Colors.lightGrey 1.0 ]
        , backgroundByRole <|
            if counterIsZero then
                Blank

            else
                Danger
        , String.fromInt options.counter
            |> text
            |> el
                [ alignTop
                , alignRight
                , Font.size 10
                , Font.color <| white 1.0
                , Background.color <| black 0.8
                , Border.rounded 10
                , padding 3
                , moveRight 4
                , moveUp 4
                , transparent counterIsZero
                ]
            |> inFront
        , renderTitle options.title
        ]
        { onPress =
            if not options.disabled then
                Just msg

            else
                Nothing
        , label =
            row [ spacing 8 ]
                [ renderIcon options.size options.icon
                , if String.isEmpty options.text then
                    none

                  else
                    text options.text
                ]
        }


button : Bool -> Html.Html msg -> msg -> Element msg
button disabled icon msg =
    Input.button
        [ padding 5
        , mouseOver <| [ Background.color <| Colors.lightGrey 1.0 ]
        , alignLeft
        , Border.rounded 5
        , width <| px 35
        ]
        { onPress =
            if not disabled then
                Just msg

            else
                Nothing
        , label = html <| icon
        }


backgroundByRole : Role -> Attr decorative msg
backgroundByRole role =
    let
        color =
            case role of
                Success ->
                    Colors.green 0.5

                Danger ->
                    Colors.orange 0.9

                Info ->
                    Colors.cyan 0.7

                Blank ->
                    Colors.white 1.0
    in
    Background.color color


renderTitle : Maybe String -> Attribute msg
renderTitle maybe_title =
    case maybe_title of
        Just title ->
            htmlAttribute <| Html.Attributes.title title

        Nothing ->
            inFront none


renderIcon : Size -> Icon -> Element msg
renderIcon size icon =
    let
        rendered =
            case icon of
                Save ->
                    Icons.save

                Download ->
                    Icons.download

                Edit ->
                    Icons.edit

                Hide ->
                    Icons.eyeOff

                Show ->
                    Icons.eye

                Add ->
                    Icons.plusCircle

                Delete ->
                    Icons.trash2

                Refresh ->
                    Icons.refreshCcw

                AddFile ->
                    Icons.filePlus

                AddTextFile ->
                    Icons.fileText

                PullRequest ->
                    Icons.gitPullRequest

                Grid ->
                    Icons.grid

                ChevronUp ->
                    Icons.chevronsUp

                None ->
                    Html.div [] []

        int =
            case icon of
                None ->
                    0

                _ ->
                    case size of
                        Small ->
                            20

                        Medium ->
                            25

                        Big ->
                            50
    in
    el [ width <| px int ] <| html rendered
