module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import EditGame
import Home
import Html exposing (div, text)
import Html.Attributes exposing (class, href)
import Route exposing (Route(..))
import Session exposing (Session)
import Url



---- MODEL ----


type Model
    = NotFound Session
    | Home Session
    | EditGame EditGame.Model
    | History Session


toKey : Model -> Nav.Key
toKey model =
    case model of
        NotFound nav ->
            nav

        Home nav ->
            nav

        EditGame model_ ->
            EditGame.toSession model_

        History nav ->
            nav


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    maybeRouteToModel (Route.fromUrl url) (Home key)



---- MSG, UPDATE ----


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotEditGameMsg EditGame.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlChanged url, _ ) ->
            maybeRouteToModel (Route.fromUrl url) model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        cmd =
                            Nav.pushUrl (toKey model) (Url.toString url)
                    in
                    maybeRouteToModel (Route.fromUrl url) model

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotEditGameMsg subMsg, EditGame subModel ) ->
            EditGame.update subMsg subModel
                |> updateWith EditGame GotEditGameMsg

        ( _, _ ) ->
            ( model, Cmd.none )


maybeRouteToModel : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
maybeRouteToModel maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.NotFound ->
            ( NotFound session, Cmd.none )

        Just Route.History ->
            ( History session, Cmd.none )

        Just (Route.EditGame gameId) ->
            updateWith
                EditGame
                GotEditGameMsg
                (EditGame.init gameId session)

        Just Route.Home ->
            ( Home session, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession model =
    case model of
        NotFound session ->
            session

        Home session ->
            session

        EditGame subModel ->
            EditGame.toSession subModel

        History session ->
            session



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

            EditGame subModel ->
                viewContainer <|
                    EditGame.view subModel

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
