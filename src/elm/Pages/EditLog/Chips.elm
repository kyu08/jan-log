module Pages.EditLog.Chips exposing
    ( Chips
    , fromDto
    , initChips
    )

import Array exposing (Array)
import Expands.Array as ExArray
import StaticArray exposing (StaticArray)
import StaticArray.Index exposing (Four)
import StaticArray.Length as Length



-- types


type alias Chips =
    StaticArray Four Chip


type alias Chip =
    String



-- functions


initChips : Chips
initChips =
    StaticArray.initialize Length.four (always "")


fromDto : Array Int -> Maybe Chips
fromDto intArray =
    case Array.toList <| ExArray.toStringArray intArray of
        head :: tail ->
            Just <| StaticArray.fromList Length.four head tail

        _ ->
            Nothing
