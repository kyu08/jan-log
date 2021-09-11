module Pages.EditLog.LogConfig exposing
    ( LogConfig
    , RankPoint
    , initLogConfig
    )

import Pages.EditLog.Rounds exposing (Point)



-- types


type alias LogConfig =
    { rate : Rate
    , chipRate : ChipRate
    , gameFee : GameFee
    , rankPoint : RankPoint

    -- 何万点持ちか
    , havePoint : Point

    -- 何万点返しか
    , returnPoint : Point
    }


type alias RankPoint =
    ( String, String )


type alias Rate =
    String


type alias ChipRate =
    String


type alias GameFee =
    String



-- functions


{-| rate : 収支 = 点数 \* n としたときの n
chipRate : チップ収支 = チップ枚数 \* m としたときの m
-}
initLogConfig : LogConfig
initLogConfig =
    { rate = "100"
    , chipRate = "2"
    , gameFee = "0"
    , rankPoint = ( "10", "20" )
    , havePoint = "25"
    , returnPoint = "30"
    }
