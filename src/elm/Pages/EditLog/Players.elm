module Pages.EditLog.Players exposing
    ( Player
    , Players
    , fromDto
    , initPlayers4
    , initPlayers5
    , toArray
    , updatePlayerName
    )

import Array exposing (Array)
import StaticArray exposing (StaticArray, fromList)
import StaticArray.Index as Index
import StaticArray.Length as Length



-- types


type Players
    = Players4 (StaticArray Index.Four Player)
    | Players5 (StaticArray Index.Five Player)


type alias Player =
    String



-- functions


initPlayers4 : Players
initPlayers4 =
    Players4 <|
        fromList
            Length.four
            "A"
            [ "B", "C", "D" ]


initPlayers5 : Players
initPlayers5 =
    Players5 <|
        fromList
            Length.five
            "A"
            [ "B", "C", "D", "E" ]


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

        Players5 players_ ->
            StaticArray.toArray players_


updatePlayerName : Int -> String -> Players -> Players
updatePlayerName playerIndex playerName players =
    case players of
        Players4 players_ ->
            Players4 <| StaticArray.set (Index.fromModBy Length.four playerIndex) playerName players_

        Players5 players_ ->
            Players5 <| StaticArray.set (Index.fromModBy Length.five playerIndex) playerName players_
