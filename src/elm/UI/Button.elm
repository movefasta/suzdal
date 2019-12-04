module UI.Button exposing (add, button, delete, next, refresh, save)

import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy exposing (lazy)
import Html exposing (Html)
import Html.Attributes
import Html.Events as HtmlEvents
import UI.Colors as Colors
import UI.Icons as Icons


type Button msg
    = Button Options msg


type alias Options =
    { role : Role
    , disabled : Bool
    , text : String
    , icon : Icon
    , size : Size
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
    }


save : msg -> Element msg
save msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon Save
        |> withSize Small
        |> renderButton


refresh : msg -> Element msg
refresh msg =
    Button defaultOptions msg
        |> withRole Info
        |> withIcon Refresh
        |> withSize Small
        |> renderButton


delete : msg -> Element msg
delete msg =
    Button defaultOptions msg
        |> withRole Danger
        |> withSize Medium
        |> withIcon Delete
        |> renderButton


add : msg -> Element msg
add msg =
    Button defaultOptions msg
        |> withRole Info
        |> withSize Medium
        |> withIcon Add
        |> withText "Добавить"
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


withRole : Role -> Button msg -> Button msg
withRole role (Button options msg) =
    Button { options | role = role } msg


disable : Button msg -> Button msg
disable (Button options msg) =
    Button { options | disabled = True } msg


renderButton : Button msg -> Element msg
renderButton (Button options msg) =
    Input.button
        [ padding 5
        , mouseOver <| backgroundByRole options.role
        , alignLeft
        , Border.rounded 5
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


backgroundByRole : Role -> List Decoration
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
    [ Background.color color ]


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
                            30

                        Big ->
                            50
    in
    el [ width <| px int ] <| html rendered
