module UI exposing
    ( viewBlank
    , viewButton
    , viewIf
    , viewLinkButton
    )

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, contenteditable, href)
import Html.Events exposing (onClick)


viewLinkButton : String -> String -> Html msg
viewLinkButton phrase pathString =
    div
        [ class "button_container" ]
        [ a [ href pathString ]
            [ div
                [ class "button_primary" ]
                [ text phrase ]
            ]
        ]


type alias ViewButtonConfig msg =
    { phrase : String
    , onClickMsg : msg
    }


viewButton : ViewButtonConfig msg -> Html msg
viewButton { phrase, onClickMsg } =
    div
        [ class "button_container", onClick onClickMsg ]
        [ div
            [ class "button_primary" ]
            [ text phrase ]
        ]


viewBlank : Html msg
viewBlank =
    div [] []


viewIf : Bool -> Html msg -> Html msg
viewIf shouldShow content =
    if shouldShow then
        content

    else
        viewBlank
