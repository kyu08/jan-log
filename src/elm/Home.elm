module Home exposing (view)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Route



-- VIEW


view : Html msg
view =
    let
        routes =
            Route.routes

        phrases =
            Route.phrases
    in
    div
        []
        [ viewButton phrases.history routes.history
        , viewButton phrases.editGame routes.editGame
        ]


type alias PathString =
    String


viewButton : String -> PathString -> Html msg
viewButton phrase pathString =
    div
        [ class "home_button_container" ]
        [ div
            [ class "home_button_primary" ]
            [ a [ href pathString ] [ text phrase ]
            ]
        ]
