module Expands.Time exposing (posixToYmdhM)

import Time exposing (Month(..))
import TimeZone exposing (asia__tokyo)


zone : Time.Zone
zone =
    asia__tokyo ()


{-| 2021/2/2
-}
posixToYmdhM : Time.Posix -> String
posixToYmdhM posix =
    (posix
        |> Time.toYear zone
        |> String.fromInt
    )
        ++ "/"
        ++ posixToMonth posix
        ++ "/"
        ++ (posix
                |> Time.toDay zone
                |> String.fromInt
           )


posixToMonth : Time.Posix -> String
posixToMonth posix =
    case Time.toMonth zone posix of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "6"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
