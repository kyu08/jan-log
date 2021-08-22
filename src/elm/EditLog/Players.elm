module EditLog.Players exposing
    ( Player
    , Players
    , initPlayers
    )

import Array exposing (Array)



-- types


type alias Players =
    Array Player


type alias Player =
    String



-- functions


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる?
-}
initPlayers : Players
initPlayers =
    Array.fromList [ "player1", "player2", "player3", "player4" ]
