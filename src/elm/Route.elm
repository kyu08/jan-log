module Route exposing (Route(..), fromUrl, parser, phrases, routes)

import GameId exposing (GameId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = NotFound
    | Home
    | EditGame GameId
    | History


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map EditGame (s routes.editGame </> string)
        , Parser.map History (s routes.history)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse
        parser
        url


routes =
    { editGame = "editGame"
    , history = "history"
    }


phrases =
    { newGame = "新規作成"
    , history = "今までの成績をみる"
    }
