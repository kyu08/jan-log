module Pages.EditLog.Dtos.LogDto exposing
    ( LogDto4
    , LogDto5
    , Round4Dto
    , Round5Dto
    , RoundDto
    )

import Array exposing (Array)


type RoundDto
    = Round4 Round4Dto
    | Round5 Round5Dto


type alias Round4Dto =
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
    , tobiSho :
        { data0 : Int
        , data1 : Int
        , data2 : Int
        , data3 : Int
        }
    }


type alias Round5Dto =
    { points :
        { data0 : Int
        , data1 : Int
        , data2 : Int
        , data3 : Int
        , data4 : Int
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
    , rounds : Array Round4Dto
    , chips : Array Int
    , rankPoint : Array Int
    , havePoint : Int
    , returnPoint : Int
    }


type alias LogDto5 =
    { createdAt : Int
    , logId : String
    , gameFee : Int
    , rate : Int
    , chipRate : Int
    , players : Array String
    , rounds : Array Round5Dto
    , chips : Array Int
    , rankPoint : Array Int
    , havePoint : Int
    , returnPoint : Int
    }
