module Pages.EditLog.Players exposing
    ( Player
    , Players
    , fromDto
    , initPlayers
    , toArray
    , updatePlayerName
    )

import Array exposing (Array)
import StaticArray exposing (StaticArray, fromList)
import StaticArray.Index as Index exposing (Four)
import StaticArray.Length as Length



-- types


type Players
    = Players4 (StaticArray Four Player)


type alias Player =
    String



-- functions


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる?
-}
initPlayers : Players
initPlayers =
    Players4 <|
        fromList
            Length.four
            "A"
            [ "B", "C", "D" ]


fromDto : Array String -> Maybe Players
fromDto dto =
    case Array.toList dto of
        head :: tail ->
            Just <| Players4 <| StaticArray.fromList Length.four head tail

        _ ->
            Nothing


toArray : Players -> Array String
toArray players =
    case players of
        Players4 players_ ->
            StaticArray.toArray players_


updatePlayerName : Int -> String -> Players -> Players
updatePlayerName playerIndex playerName players =
    case players of
        Players4 players_ ->
            Players4 <| StaticArray.set (Index.fromModBy Length.four playerIndex) playerName players_
