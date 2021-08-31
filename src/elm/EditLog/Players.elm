module EditLog.Players exposing
    ( Player
    , Players
    , fromDto
    , initPlayers
    )

import Array exposing (Array)
import StaticArray exposing (StaticArray, fromList)
import StaticArray.Index exposing (Four)
import StaticArray.Length as Length



-- types


type alias Players =
    StaticArray Four Player


type alias Player =
    String



-- functions


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる?
-}
initPlayers : Players
initPlayers =
    fromList
        Length.four
        "A"
        [ "B", "C", "D" ]


fromDto : Array String -> Maybe Players
fromDto dto =
    case Array.toList dto of
        head :: tail ->
            Just <| StaticArray.fromList Length.four head tail

        _ ->
            Nothing
