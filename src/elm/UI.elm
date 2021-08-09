module UI exposing
    ( Size(..)
    , viewBlank
    , viewButton
    , viewIf
    , viewLinkButton
    , viewModal
    )

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, contenteditable, href)
import Html.Events exposing (onClick)


type Size
    = Default
    | Mini


type alias ViewButtonConfig msg =
    { phrase : String
    , onClickMsg : msg
    , size : Size
    }


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


viewButton : ViewButtonConfig msg -> Html msg
viewButton { phrase, onClickMsg, size } =
    let
        containerClass =
            case size of
                Default ->
                    "button_container"

                Mini ->
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


viewModal : Html msg -> Html msg
viewModal content =
    div [ class "modal_tranceparentLayer" ]
        [ div
            [ class "modal_container" ]
            [ content ]
        ]


viewIf : Bool -> Html msg -> Html msg
viewIf shouldShow content =
    if shouldShow then
        content

    else
        viewBlank
