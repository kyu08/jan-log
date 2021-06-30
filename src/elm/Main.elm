module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Route exposing (Route)



---- MODEL ----


type Model
    = NotFound
    | Home
    | NewGame


init : ( Model, Cmd Msg )
init =
    ( Home, Cmd.none )



---- MSG, UPDATE ----


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


changeToRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
changeToRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.NotFound ->
            ( NotFound, Cmd.none )

        Just Route.History ->
            ( NotFound, Cmd.none )

        Just Route.NewGame ->
            ( NotFound, Cmd.none )

        Just Route.Home ->
            ( NotFound, Cmd.none )



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
