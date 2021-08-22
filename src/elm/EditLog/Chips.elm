module EditLog.Chips exposing
    ( Chips
    , initChips
    )

import Array exposing (Array)



-- types


type alias Chips =
    Array Chip


type alias Chip =
    String



-- functions


initChips : Chips
initChips =
    Array.initialize
        4
        (always "")
