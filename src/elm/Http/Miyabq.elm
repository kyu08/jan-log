module Http.Miyabq exposing
    ( getUsers
    , postResult
    )

import Http
import IO.Miyabq exposing (resultsEncoder, userDecoder)
import Json.Decode as D
import Pages.EditLog.Dtos.ResultsDto exposing (ResultsDto)
import Pages.EditLog.Dtos.UserDto exposing (UserDto)


type alias PostResultConfig msg =
    { resultsDto : ResultsDto
    , onResponseMsg : Result Http.Error String -> msg
    }


getUsers : (Result Http.Error (List UserDto) -> msg) -> Cmd msg
getUsers gotMsg =
    Http.get
        { url = miyabqBaseUrl ++ "/users"
        , expect = Http.expectJson gotMsg (D.list userDecoder)
        }


postResult : PostResultConfig msg -> Cmd msg
postResult postResultConfig =
    Http.post
        { url = miyabqBaseUrl ++ "/result_json"
        , body = Http.jsonBody <| resultsEncoder postResultConfig.resultsDto
        , expect = Http.expectString postResultConfig.onResponseMsg
        }



-- internal


miyabqBaseUrl : String
miyabqBaseUrl =
    "https://asia-northeast1-miyabq.cloudfunctions.net/mahjong"
