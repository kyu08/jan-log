module Route exposing (Route(..), fromUrl, parser, routes)

import Common.LogId exposing (LogId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = NotFound
    | Home
    | EditLog LogId
    | History


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map EditLog (s routes.editLog4 </> string)
        , Parser.map History (s routes.history)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse
        parser
        url


routes =
    { editLog4 = "editLog4"
    , history = "history"
    , home = "/"
    }
