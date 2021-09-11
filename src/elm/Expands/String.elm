module Expands.String exposing
    ( fromInt
    , toIntValue
    )


toIntValue : String -> Int
toIntValue string =
    Maybe.withDefault 0 (String.toInt string)


fromInt : Int -> String
fromInt int =
    if int == 0 then
        ""

    else
        String.fromInt int
