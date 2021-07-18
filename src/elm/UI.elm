module UI exposing (viewButton)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Route exposing (PathString)


viewButton : String -> PathString -> Html msg
viewButton phrase pathString =
    div
        [ class "button_container" ]
        [ a [ href pathString ]
            [ div
                [ class "button_primary" ]
                [ text phrase ]
            ]
        ]
