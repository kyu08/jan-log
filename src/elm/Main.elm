module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import EditGame
import Home
import Html exposing (div, text)
import Html.Attributes exposing (class, href)
import Route exposing (Route(..))
import Session exposing (Session)
import UUID exposing (UUID)
import Url



---- MODEL ----


type Model
    = NotFound Session
    | Home Home.Model
    | EditGame EditGame.Model
    | History Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    maybeRouteToModel url (Home <| Home.initModel key)


toSession : Model -> Session
toSession model =
    case model of
        NotFound session ->
            session

        Home subModel ->
            Home.toSession subModel

        EditGame subModel ->
            EditGame.toSession subModel

        History session ->
            session



---- MSG, UPDATE ----


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | GotEditGameMsg EditGame.Msg
    | GotHomeMsg Home.Msg
    | UUIDGenerated UUID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (toSession model) (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            maybeRouteToModel url model

        ( GotEditGameMsg subMsg, EditGame subModel ) ->
            EditGame.update subMsg subModel
                |> updateWith EditGame GotEditGameMsg

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg

        ( _, _ ) ->
            ( model, Cmd.none )


maybeRouteToModel : Url.Url -> Model -> ( Model, Cmd Msg )
maybeRouteToModel url model =
    let
        maybeRoute =
            Route.fromUrl url

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
            ( EditGame (EditGame.initModel gameId session), Cmd.none )

        Just Route.Home ->
            let
                ( model_, cmd ) =
                    updateWith
                        Home
                        GotHomeMsg
                        (Home.init session)
            in
            ( model_, cmd )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



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

            Home subModel ->
                viewContainer <|
                    Home.view subModel

            EditGame subModel ->
                viewContainer <|
                    Html.map GotEditGameMsg <|
                        EditGame.view
                            { gameConfig = subModel.gameConfig
                            , players = subModel.players
                            , rounds = subModel.rounds
                            }

            History _ ->
                viewContainer <|
                    div
                        [ class "main_container" ]
                        [ text "history"
                        ]
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
