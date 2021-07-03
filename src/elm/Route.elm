module Route exposing (Route(..), fromUrl, parser, phrases, routes)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = NotFound
    | Home
    | NewGame
    | History


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map NewGame (s routes.newGame)
        , Parser.map History (s routes.history)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse
        parser
        url


routes =
    { newGame = "newGame"
    , history = "history"
    }


phrases =
    { newGame = "新規作成"
    , history = "今までの成績をみる"
    }
