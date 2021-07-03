module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Home
import Html exposing (div, text)
import Html.Attributes exposing (class, href)
import Route
import Url



---- MODEL ----


type Model
    = NotFound Nav.Key
    | Home Nav.Key
    | NewGame Nav.Key
    | History Nav.Key


toKey : Model -> Nav.Key
toKey model =
    case model of
        NotFound nav ->
            nav

        Home nav ->
            nav

        NewGame nav ->
            nav

        History nav ->
            nav


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( Home key, Cmd.none )



---- MSG, UPDATE ----


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( changeRouteTo (Route.fromUrl url) model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        cmd =
                            Nav.pushUrl (toKey model) (Url.toString url)
                    in
                    ( changeRouteTo (Route.fromUrl url) model, cmd )

                Browser.External href ->
                    ( model, Nav.load href )


changeRouteTo : Maybe Route.Route -> Model -> Model
changeRouteTo maybeRoute model =
    let
        key =
            toKey model
    in
    case maybeRoute of
        Nothing ->
            NotFound key

        Just Route.NotFound ->
            NotFound key

        Just Route.History ->
            History key

        Just Route.NewGame ->
            NewGame key

        Just Route.Home ->
            Home key



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        viewContainer content =
            [ div [ class "container" ] [ content ] ]
    in
    { title = "Jan-Log"
    , body =
        case model of
            NotFound _ ->
                viewContainer <| text "not found"

            Home _ ->
                viewContainer <|
                    Home.view

            NewGame _ ->
                viewContainer <|
                    div
                        [ class "main_container" ]
                        [ text "newGame"
                        ]

            History _ ->
                viewContainer <|
                    div
                        [ class "main_container" ]
                        [ text "history"
                        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
