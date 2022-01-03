module Expands.Maybe exposing (isJust)

import Json.Decode exposing (maybe)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
