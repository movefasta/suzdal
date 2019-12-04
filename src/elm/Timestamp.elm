module Timestamp exposing (format, view)

import Element exposing (Element, text)
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Time exposing (Month(..))



-- VIEW


view : Time.Zone -> Time.Posix -> Element msg
view timeZone timestamp =
    text (format timeZone timestamp)



-- FORMAT


{-| Format a timestamp as a String, like so:

    "12:00 07.09.2019"

For more complex date formatting scenarios, here's a nice package:
<https://package.elm-lang.org/packages/ryannhg/date-format/latest/>

-}
format : Time.Zone -> Time.Posix -> String
format zone time =
    let
        month =
            case Time.toMonth zone time of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        minute =
            Time.toMinute Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            Time.toHour Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'

        day =
            Time.toDay Time.utc time
                |> String.fromInt
                |> String.padLeft 2 '0'

        year =
            String.fromInt (Time.toYear Time.utc time)
    in
    "[ " ++ hour ++ ":" ++ minute ++ " " ++ day ++ "." ++ month ++ "." ++ year ++ " ]"
