module LogDto exposing
    ( LogDto4
    , RoundObj4
    )

import Array exposing (Array)


type alias RoundObj4 =
    { data0 : Int
    , data1 : Int
    , data2 : Int
    , data3 : Int
    }


type alias LogDto4 =
    { logId : String
    , gameFee : Int
    , rate : Int
    , chipRate : Int
    , players : Array String
    , rounds : Array RoundObj4
    , chips : Array Int
    , rankPoint : Array Int
    , topBonus : Int
    }
