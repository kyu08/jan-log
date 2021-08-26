module Dtos.LogDto exposing
    ( LogDto4
    , RoundObj4Dto
    )

import Array exposing (Array)


type alias RoundObj4Dto =
    { points :
        { data0 : Int
        , data1 : Int
        , data2 : Int
        , data3 : Int
        }
    , seatingOrder :
        Maybe
            { ton : Int
            , nan : Int
            , sha : Int
            , pei : Int
            }
    }


type alias LogDto4 =
    { createdAt : Int
    , logId : String
    , gameFee : Int
    , rate : Int
    , chipRate : Int
    , players : Array String
    , rounds : Array RoundObj4Dto
    , chips : Array Int
    , rankPoint : Array Int
    , havePoint : Int
    , returnPoint : Int
    }
