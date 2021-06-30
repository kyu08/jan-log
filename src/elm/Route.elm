module Route exposing (Route(..), parser)

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
        , Parser.map NewGame (s "newGame")
        , Parser.map History (s "history")
        ]
