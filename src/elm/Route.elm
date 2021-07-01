module Route exposing (Route(..), fromUrl, parser)

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
        , Parser.map NewGame (s "newGame")
        , Parser.map History (s "history")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
