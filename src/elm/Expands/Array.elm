module Expands.Array exposing
    ( getArrayElement
    , toIntArray
    , toStringArray
    )

import Array exposing (Array)
import Expands.String as ExString


toIntArray : Array String -> Array Int
toIntArray stringArray =
    Array.map
        ExString.toIntValue
        stringArray


getArrayElement : Int -> Array Int -> Int
getArrayElement index array =
    Maybe.withDefault 0 <|
        Array.get
            index
            array


toStringArray : Array Int -> Array String
toStringArray arrayInt =
    Array.map String.fromInt arrayInt
