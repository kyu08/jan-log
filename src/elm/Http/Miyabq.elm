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
        { url = localhost ++ "/result_json"

        -- 開発完了したら戻す
        -- { url = miyabqBaseUrl ++ "/result_json"
        , body = Http.jsonBody <| resultsEncoder postResultConfig.resultsDto
        , expect = Http.expectJson postResultConfig.onResponseMsg D.string
        }



-- internal


miyabqBaseUrl : String
miyabqBaseUrl =
    "https://asia-northeast1-miyabq.cloudfunctions.net/mahjong"


localhost =
    "http://localhost:3000/"
