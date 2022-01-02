module IO.Miyabq exposing (..)

import Array exposing (Array)
import Json.Decode as D
import Json.Encode as E


type alias LogResult =
    { match_date : String
    , results : Array Hoge
    }


type alias Hoge =
    { user_id : String
    , scores : Array Int
    , chip : Int
    }


resultDecoder_ : LogResult -> E.Value
resultDecoder_ logResult =
    E.object
        [ ( "match_date", E.string logResult.match_date )
        , ( "results", E.array resultDecoder logResult.results )
        ]


resultDecoder : Hoge -> E.Value
resultDecoder result =
    E.object
        [ ( "user_id", E.string result.user_id )
        , ( "scores", E.array E.int result.scores )
        , ( "chip", E.int result.chip )
        ]
