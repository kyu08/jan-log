module UI exposing
    ( Size(..)
    , viewBlank
    , viewButton
    , viewIf
    , viewLinkButton
    )

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
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


type Size
    = Default
    | Mini


type alias ViewButtonConfig msg =
    { phrase : String
    , onClickMsg : msg
    , size : Size
    }


viewButton : ViewButtonConfig msg -> Html msg
viewButton { phrase, onClickMsg, size } =
    let
        containerClass =
            if size == Default then
                "button_container"

            else
                "button_container_mini"
    in
    div
        [ class containerClass, onClick onClickMsg ]
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
