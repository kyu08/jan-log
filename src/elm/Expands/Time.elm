module Expands.Time exposing
    ( posixToYmd
    , posixToYmdWithHyphenDelimiter
    )

import Time exposing (Month(..))
import TimeZone exposing (asia__tokyo)


zone : Time.Zone
zone =
    asia__tokyo ()


{-| 2021/2/2
-}
posixToYmd : Time.Posix -> String
posixToYmd posix =
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


{-| 2021/02/02
-}
posixToYmdWithHyphenDelimiter : Time.Posix -> String
posixToYmdWithHyphenDelimiter posix =
    (posix
        |> Time.toYear zone
        |> String.fromInt
    )
        ++ "-"
        ++ (posixToMonth posix
                |> toDoubleDegit
           )
        ++ "-"
        ++ (posix
                |> Time.toDay zone
                |> String.fromInt
                |> toDoubleDegit
           )


toDoubleDegit : String -> String
toDoubleDegit str =
    case String.length str of
        1 ->
            "0" ++ str

        _ ->
            str


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
