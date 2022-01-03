module IO.Miyabq exposing
    ( resultsEncoder
    , userDecoder
    )

import Json.Decode as D
import Json.Encode as E
import Pages.EditLog.Dtos.ResultsDto exposing (ResultDto, ResultsDto)
import Pages.EditLog.Dtos.UserDto exposing (UserDto)


userDecoder : D.Decoder UserDto
userDecoder =
    D.map2 UserDto
        (D.field "id" D.int)
        (D.field "name" D.string)


resultsEncoder : ResultsDto -> E.Value
resultsEncoder resultsDto =
    E.object
        [ ( "match_date", E.string resultsDto.match_date )
        , ( "results", E.list resultEncoder resultsDto.results )
        ]


resultEncoder : ResultDto -> E.Value
resultEncoder resultDto =
    E.object
        [ ( "user_id", E.int resultDto.user_id )
        , ( "scores", E.array E.int resultDto.scores )
        , ( "chip", E.int resultDto.chip )
        ]
