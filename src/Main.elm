module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = ClickedHistoryButton
    | ClickedNewButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHistoryButton ->
            ( Debug.log "model" model, Cmd.none )

        ClickedNewButton ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "main_container" ]
        [ viewButton "今までの結果をみる" ClickedHistoryButton
        , viewButton "新規作成" ClickedNewButton
        ]


viewButton : String -> Msg -> Html Msg
viewButton phrase clickedMsg =
    div [ class "button_container", onClick clickedMsg ]
        [ div [ class "button_primary" ] [ text phrase ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
