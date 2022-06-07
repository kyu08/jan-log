module Expands.Html exposing (stringToHtmlIncludingBr)

import Html exposing (Html, br, text)


{-| 改行コードを含む文字列を br をふくんだ List(Html msg)に
-}
stringToHtmlIncludingBr : String -> List (Html msg)
stringToHtmlIncludingBr str =
    str
        |> String.lines
        |> List.map text
        |> List.intersperse (br [] [])
