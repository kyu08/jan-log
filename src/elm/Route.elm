module Route exposing (Route(..), fromUrl, parser, routes)

import Common.LogId exposing (LogId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = NotFound
    | Home
    | EditLog4 LogId
    | EditLog5 LogId
    | History


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map EditLog4 (s routes.editLog4 </> string)
        , Parser.map EditLog5 (s routes.editLog5 </> string)
        , Parser.map History (s routes.history)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse
        parser
        url


routes =
    { editLog4 = "editLog4"
    , editLog5 = "editLog5"
    , history = "history"
    , home = "/"
    }
