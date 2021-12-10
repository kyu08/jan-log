module Pages.EditLog.Chips exposing
    ( Chips
    , fromDto
    , initChips4
    , initChips5
    , toArray
    , toList
    , update
    )

import Array exposing (Array)
import Expands.Array as ExArray
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length



-- types


type Chips
    = Chips4 (StaticArray Index.Four Chip)
    | Chips5 (StaticArray Index.Five Chip)


type alias Chip =
    String


update : Int -> Chip -> Chips -> Chips
update playerIndex chip chips =
    case chips of
        Chips4 chips4 ->
            Chips4 <|
                StaticArray.set
                    (Index.fromModBy Length.four playerIndex)
                    chip
                    chips4

        Chips5 chips5 ->
            Chips5 <|
                StaticArray.set
                    (Index.fromModBy Length.five playerIndex)
                    chip
                    chips5



-- functions


initChips4 : Chips
initChips4 =
    Chips4 <| StaticArray.initialize Length.four (always "")


initChips5 : Chips
initChips5 =
    Chips5 <| StaticArray.initialize Length.five (always "")


fromDto : Array Int -> Maybe Chips
fromDto intArray =
    case Array.toList <| ExArray.toStringArray intArray of
        head :: tail ->
            Just <| Chips4 <| StaticArray.fromList Length.four head tail

        _ ->
            Nothing


toArray : Chips -> Array Chip
toArray chips =
    case chips of
        Chips4 chips4 ->
            StaticArray.toArray chips4

        Chips5 chips5 ->
            StaticArray.toArray chips5


toList : Chips -> List Chip
toList chips =
    case chips of
        Chips4 chips4 ->
            StaticArray.toList chips4

        Chips5 chips5 ->
            StaticArray.toList chips5
