module Debug.ForIphoneDebug exposing (log)

import Array


round =
    { points =
        { data0 = 10
        , data1 = 11
        , data2 = 12
        , data3 = 9
        }
    , seatingOrder = Nothing
    }


log =
    { createdAt = 0
    , logId = "a"
    , gameFee = 10
    , rate = 20
    , chipRate = 30
    , players = Array.fromList [ "a", "b", "c", "d" ]
    , rounds = Array.fromList [ round, round, round, round ]
    , chips = Array.fromList [ 10, 20, 10, 20 ]
    , rankPoint = Array.fromList [ 10, 20 ]
    , havePoint = 20
    , returnPoint = 30
    }
