module EditLog.Log exposing
    ( Log
    , initLog
    )

import EditLog.Chips as Chips exposing (Chips)
import EditLog.LogConfig as LogConfig exposing (LogConfig)
import EditLog.Players as Players exposing (Players)
import EditLog.Rounds as Rounds exposing (Rounds)
import Time


{-| 現状 EditLog.elm で更新も行っているが、EditLog が更新方法の詳細を知っている必要はないのでこのモジュールでやるべき。
-}
type alias Log =
    { createdAt : Time.Posix
    , players : Players
    , logConfig : LogConfig
    , rounds : Rounds
    , chips : Chips
    }


initLog : Time.Posix -> Log
initLog currentTime =
    { createdAt = currentTime
    , players = Players.initPlayers
    , logConfig = LogConfig.initLogConfig
    , rounds = Rounds.initRounds
    , chips = Chips.initChips
    }
