module Pages.EditLog.Dtos.ResultsDto exposing
    ( ResultDto
    , ResultsDto
    )

import Array exposing (Array)


type alias ResultsDto =
    { match_date : String
    , results : List ResultDto
    }


type alias ResultDto =
    { user_id : Int
    , scores : Array Int
    , chip : Int
    }
