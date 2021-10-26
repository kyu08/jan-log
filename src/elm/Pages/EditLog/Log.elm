module Pages.EditLog.Log exposing
    ( Log
    , dto4ToLog
    , initLog
    , toLogDto4
    )

import Array
import Common.LogId exposing (LogId)
import Expands.Array as ExArray
import Expands.String as ExString
import Pages.EditLog.Chips as Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (LogDto4)
import Pages.EditLog.LogConfig as LogConfig exposing (LogConfig)
import Pages.EditLog.Players as Players exposing (Players)
import Pages.EditLog.Rounds as Rounds exposing (Rounds)
import StaticArray
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



-- Dto


dto4ToLog : LogDto4 -> Maybe Log
dto4ToLog logDto4 =
    Maybe.map2
        (\players_ chips_ ->
            { createdAt = Time.millisToPosix logDto4.createdAt
            , players = players_
            , logConfig =
                { rate = String.fromInt logDto4.rate
                , chipRate = String.fromInt logDto4.chipRate
                , gameFee = String.fromInt logDto4.gameFee
                , rankPoint =
                    Tuple.pair
                        (String.fromInt <| ExArray.getArrayElement 0 logDto4.rankPoint)
                        (String.fromInt <| ExArray.getArrayElement 1 logDto4.rankPoint)
                , havePoint = String.fromInt logDto4.havePoint
                , returnPoint = String.fromInt logDto4.returnPoint
                }
            , rounds = Array.map Rounds.roundFromDto logDto4.rounds
            , chips = chips_
            }
        )
        (Players.fromDto logDto4.players)
        (Chips.fromDto logDto4.chips)


toLogDto4 : LogId -> Log -> LogDto4
toLogDto4 logId log =
    { createdAt = Time.posixToMillis log.createdAt
    , logId = logId
    , players = Players.toArray log.players
    , rate = ExString.toIntValue log.logConfig.rate
    , chipRate = ExString.toIntValue log.logConfig.chipRate
    , gameFee = ExString.toIntValue log.logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toIntValue <| Tuple.first log.logConfig.rankPoint, ExString.toIntValue <| Tuple.second log.logConfig.rankPoint ]
    , havePoint = ExString.toIntValue log.logConfig.havePoint
    , returnPoint = ExString.toIntValue log.logConfig.returnPoint
    , rounds = Array.map Rounds.toRoundObj4 log.rounds
    , chips =
        log.chips
            |> StaticArray.toArray
            |> ExArray.toIntArray
    }
