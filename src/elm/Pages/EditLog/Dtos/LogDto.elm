module Pages.EditLog.Dtos.LogDto exposing
    ( LogDto4
    , LogDto5
    , Round4DtoValue
    , Round5DtoValue
    , RoundDto(..)
    )

import Array exposing (Array)


type RoundDto
    = Round4Dto Round4DtoValue
    | Round5Dto Round5DtoValue


type alias Round4DtoValue =
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


type alias Round5DtoValue =
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
    , tobiSho :
        { data0 : Int
        , data1 : Int
        , data2 : Int
        , data3 : Int
        , data4 : Int
        }
    }


type alias LogDto4 =
    { createdAt : Int
    , logId : String
    , gameFee : Int
    , rate : Int
    , chipRate : Int
    , players : Array String
    , rounds : Array Round4DtoValue
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
    , rounds : Array Round5DtoValue
    , chips : Array Int
    , rankPoint : Array Int
    , havePoint : Int
    , returnPoint : Int
    }
