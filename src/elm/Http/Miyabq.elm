module Http.Miyabq exposing (postResult)

import Http
import IO.Miyabq exposing (resultsDecoder)
import Json.Decode as D
import Pages.EditLog.Dtos.ResultsDto exposing (ResultsDto)


type alias PostResultConfig msg =
    { resultsDto : ResultsDto
    , onResponseMsg : Result Http.Error String -> msg
    }


postResult : PostResultConfig msg -> Cmd msg
postResult postResultConfig =
    Http.post
        { url = miyabqBaseUrl ++ "/result_json"
        , body = Http.jsonBody <| resultsDecoder postResultConfig.resultsDto
        , expect = Http.expectJson postResultConfig.onResponseMsg D.string
        }



-- internal


miyabqBaseUrl : String
miyabqBaseUrl =
    "https://asia-northeast1-miyabq.cloudfunctions.net/mahjong"
