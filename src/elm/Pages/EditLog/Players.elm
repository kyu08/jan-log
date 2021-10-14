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
import StaticArray.Index as Index
import StaticArray.Length as Length



-- types


type Players
    = Players4 (StaticArray Index.Four Player)
    | Players5 (StaticArray Index.Five Player)


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

        Players5 players_ ->
            StaticArray.toArray players_


updatePlayerName : Int -> String -> Players -> Players
updatePlayerName playerIndex playerName players =
    case players of
        Players4 players_ ->
            Players4 <| StaticArray.set (Index.fromModBy Length.four playerIndex) playerName players_

        Players5 players_ ->
            Players5 <| StaticArray.set (Index.fromModBy Length.five playerIndex) playerName players_
