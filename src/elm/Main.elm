module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, img, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Route exposing (Route(..))
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
init flag url key =
    ( Home key, Cmd.none )



---- MSG, UPDATE ----


type Msg
    = ClickedHistoryButton
    | ClickedNewButton
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHistoryButton ->
            ( model, Cmd.none )

        ClickedNewButton ->
            ( model, Cmd.none )

        UrlChanged url ->
            changeToRoute (Route.fromUrl url) model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


changeToRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
changeToRoute maybeRoute model =
    let
        key =
            toKey model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound key, Cmd.none )

        Just Route.NotFound ->
            ( NotFound key, Cmd.none )

        Just Route.History ->
            ( History key, Cmd.none )

        Just Route.NewGame ->
            ( NewGame key, Cmd.none )

        Just Route.Home ->
            ( Home key, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Jan-Log"
    , body =
        [ div
            [ class "main_container" ]
            [ viewButton "今までの結果をみる"
            , viewButton "新規作成"
            ]
        ]
    }



-- TODO: a tag ベースで実装しなおす！


viewButton : String -> Html Msg
viewButton phrase =
    div
        [ class "button_container" ]
        [ div
            [ class "button_primary" ]
            [ a [ href "/hoge" ] [ text phrase ]
            ]
        ]


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
