module IO.Miyabq exposing (resultsDecoder)

import Array exposing (Array)
import Json.Encode as E


type alias ResultsDto =
    { match_date : String
    , results : List ResultDto
    }


type alias ResultDto =
    { user_id : Int
    , scores : Array Int
    , chip : Int
    }


resultsDecoder : ResultsDto -> E.Value
resultsDecoder resultsDto =
    E.object
        [ ( "match_date", E.string resultsDto.match_date )
        , ( "results", E.list resultDecoder resultsDto.results )
        ]


resultDecoder : ResultDto -> E.Value
resultDecoder resultDto =
    E.object
        [ ( "user_id", E.int resultDto.user_id )
        , ( "scores", E.array E.int resultDto.scores )
        , ( "chip", E.int resultDto.chip )
        ]
