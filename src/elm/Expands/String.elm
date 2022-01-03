module Expands.String exposing
    ( fromInt
    , toInt
    )


toInt : String -> Int
toInt string =
    Maybe.withDefault 0 (String.toInt string)


fromInt : Int -> String
fromInt int =
    if int == 0 then
        ""

    else
        String.fromInt int
