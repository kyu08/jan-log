module Expands.String exposing (toIntValue)


toIntValue : String -> Int
toIntValue string =
    Maybe.withDefault 0 (String.toInt string)
