module LogDto exposing
    ( LogDto4
    , RoundObj4
    )

import Array exposing (Array)


type alias RoundObj4 =
    { points :
        { data0 : Int
        , data1 : Int
        , data2 : Int
        , data3 : Int
        }
    , chicha : Maybe Int
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
    , havePoint : Int
    , returnPoint : Int
    }
