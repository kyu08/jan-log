module IO.Miyabq exposing (resultsDecoder)

import Json.Encode as E
import Pages.EditLog.Dtos.ResultsDto exposing (ResultDto, ResultsDto)


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
