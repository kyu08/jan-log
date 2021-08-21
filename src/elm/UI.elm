module UI exposing
    ( Size(..)
    , viewBlank
    , viewButton
    , viewIf
    , viewLinkButton
    , viewModal
    )

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, classList, contenteditable, disabled, href)
import Html.Events exposing (onClick)


type Size
    = Default
    | Mini


type alias ViewButtonConfig msg =
    { phrase : String
    , onClickMsg : msg
    , size : Size
    , isDisabled : Bool
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
viewButton { phrase, onClickMsg, size, isDisabled } =
    let
        containerClass =
            case size of
                Default ->
                    "button_container"

                Mini ->
                    "button_container_mini"
    in
    div
        [ classList [ ( containerClass, True ) ]
        , onClick onClickMsg
        ]
        [ div
            [ classList [ ( "button_primary", True ), ( "is-disabled", isDisabled ) ] ]
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
