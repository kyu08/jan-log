module Pages.EditLog.Log exposing
    ( Log
    , dto4ToLog
    , dto5ToLog
    , initLog4
    , initLog5
    , toLogDto4
    , toLogDto5
    , toResultDto4
    )

import Array exposing (Array)
import Common.LogId exposing (LogId)
import Expands.Array as ExArray
import Expands.String as ExString
import Expands.Time as ExTime
import Pages.EditLog.Chips as Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (LogDto4, LogDto5)
import Pages.EditLog.Dtos.ResultsDto exposing (ResultsDto)
import Pages.EditLog.LogConfig as LogConfig exposing (LogConfig)
import Pages.EditLog.Players as Players exposing (Players)
import Pages.EditLog.Rounds as Rounds exposing (Rounds)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length
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


initLog4 : Time.Posix -> Log
initLog4 currentTime =
    { createdAt = currentTime
    , players = Players.initPlayers4
    , logConfig = LogConfig.initLogConfig
    , rounds = Rounds.initRounds4
    , chips = Chips.initChips4
    }


initLog5 : Time.Posix -> Log
initLog5 currentTime =
    { createdAt = currentTime
    , players = Players.initPlayers5
    , logConfig = LogConfig.initLogConfig
    , rounds = Rounds.initRounds5
    , chips = Chips.initChips5
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
            , rounds = Array.map Rounds.round4FromDto logDto4.rounds
            , chips = chips_
            }
        )
        (Players.fromDto logDto4.players)
        (Chips.fromDto logDto4.chips)


dto5ToLog : LogDto5 -> Maybe Log
dto5ToLog logDto5 =
    Maybe.map2
        (\players_ chips_ ->
            { createdAt = Time.millisToPosix logDto5.createdAt
            , players = players_
            , logConfig =
                { rate = String.fromInt logDto5.rate
                , chipRate = String.fromInt logDto5.chipRate
                , gameFee = String.fromInt logDto5.gameFee
                , rankPoint =
                    Tuple.pair
                        (String.fromInt <| ExArray.getArrayElement 0 logDto5.rankPoint)
                        (String.fromInt <| ExArray.getArrayElement 1 logDto5.rankPoint)
                , havePoint = String.fromInt logDto5.havePoint
                , returnPoint = String.fromInt logDto5.returnPoint
                }
            , rounds = Array.map Rounds.round5FromDto logDto5.rounds
            , chips = chips_
            }
        )
        (Players.fromDto logDto5.players)
        (Chips.fromDto logDto5.chips)


toLogDto4 : LogId -> Log -> LogDto4
toLogDto4 logId log =
    { createdAt = Time.posixToMillis log.createdAt
    , logId = logId
    , players = Players.toArray log.players
    , rate = ExString.toInt log.logConfig.rate
    , chipRate = ExString.toInt log.logConfig.chipRate
    , gameFee = ExString.toInt log.logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toInt <| Tuple.first log.logConfig.rankPoint, ExString.toInt <| Tuple.second log.logConfig.rankPoint ]
    , havePoint = ExString.toInt log.logConfig.havePoint
    , returnPoint = ExString.toInt log.logConfig.returnPoint
    , rounds = Array.map Rounds.toRound4Dto log.rounds
    , chips =
        log.chips
            |> Chips.toArray
            |> ExArray.toIntArray
    }


toLogDto5 : LogId -> Log -> LogDto5
toLogDto5 logId log =
    { createdAt = Time.posixToMillis log.createdAt
    , logId = logId
    , players = Players.toArray log.players
    , rate = ExString.toInt log.logConfig.rate
    , chipRate = ExString.toInt log.logConfig.chipRate
    , gameFee = ExString.toInt log.logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toInt <| Tuple.first log.logConfig.rankPoint, ExString.toInt <| Tuple.second log.logConfig.rankPoint ]
    , havePoint = ExString.toInt log.logConfig.havePoint
    , returnPoint = ExString.toInt log.logConfig.returnPoint
    , rounds = Array.map Rounds.toRound5Dto log.rounds
    , chips =
        log.chips
            |> Chips.toArray
            |> ExArray.toIntArray
    }


type alias ToResultDto4Config =
    { createdAt : Time.Posix
    , playerIds : StaticArray Index.Four Int
    , rounds : Rounds
    , chips : Array Int
    , rankPoint : ( Int, Int )
    , returnPoint : Int
    }


toResultDto4 : ToResultDto4Config -> ResultsDto
toResultDto4 toResultDto4Config =
    { match_date = ExTime.posixToYmdWithHyphenDelimiter toResultDto4Config.createdAt
    , results =
        toResultDto4Config.playerIds
            |> StaticArray.indexedMap
                (\index playerId ->
                    { user_id = playerId
                    , scores =
                        Rounds.toScores
                            { index = Index.toInt index
                            , rankPoint = toResultDto4Config.rankPoint
                            , returnPoint = toResultDto4Config.returnPoint
                            , rounds = toResultDto4Config.rounds
                            }
                    , chip =
                        -- FIXME: 0なのかエラーなのかわからないのでよくない
                        Maybe.withDefault 0 <|
                            Array.get
                                (Index.toInt index)
                                toResultDto4Config.chips
                    }
                )
            |> StaticArray.toList
    }
