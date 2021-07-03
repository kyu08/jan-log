module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import EditGame
import Home
import Html exposing (div, text)
import Html.Attributes exposing (class, href)
import Route
import Url



---- MODEL ----


type Model
    = NotFound Nav.Key
    | Home Nav.Key
    | EditGame Nav.Key
    | History Nav.Key


toKey : Model -> Nav.Key
toKey model =
    case model of
        NotFound nav ->
            nav

        Home nav ->
            nav

        EditGame nav ->
            nav

        History nav ->
            nav


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( maybeRouteToModel (Route.fromUrl url) key, Cmd.none )



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


maybeRouteToModel : Maybe Route.Route -> (Nav.Key -> Model)
maybeRouteToModel maybeRoute =
    case maybeRoute of
        Nothing ->
            NotFound

        Just Route.NotFound ->
            NotFound

        Just Route.History ->
            History

        Just Route.EditGame ->
            EditGame

        Just Route.Home ->
            Home


changeRouteTo : Maybe Route.Route -> Model -> Model
changeRouteTo maybeRoute model =
    maybeRouteToModel maybeRoute <| toKey model



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

            EditGame _ ->
                viewContainer <|
                    EditGame.view

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
