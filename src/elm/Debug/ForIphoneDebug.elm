module Debug.ForIphoneDebug exposing (log)

import Array


round : { points : { data0 : number, data1 : number, data2 : number, data3 : number }, seatingOrder : Maybe a }
round =
    { points =
        { data0 = 10
        , data1 = 11
        , data2 = 12
        , data3 = 9
        }
    , seatingOrder = Nothing
    }


log :
    { createdAt : number
    , logId : String
    , gameFee : number
    , rate : number
    , chipRate : number
    , players : Array.Array String
    , rounds : Array.Array { points : { data0 : number, data1 : number, data2 : number, data3 : number }, seatingOrder : Maybe a }
    , chips : Array.Array number
    , rankPoint : Array.Array number
    , havePoint : number
    , returnPoint : number
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
