module Pages.Home exposing
    ( Model
    , Msg
    , init
    , initModel
    , toSession
    , update
    , view
    )

import Html exposing (Html, div)
import Random
import Route exposing (Route(..))
import Session exposing (Session)
import UI
import UUID exposing (UUID)


type alias Model =
    { session : Session
    , newLogId : UUIDS
    }


type UUIDS
    = Generating
    | Generated UUID


initModel : Session -> Model
initModel session =
    { session = session, newLogId = Generating }


initCmd : Cmd Msg
initCmd =
    Random.generate UUIDGenerated UUID.generator


init : Session -> ( Model, Cmd Msg )
init session =
    ( initModel session, initCmd )


toSession : Model -> Session
toSession model =
    model.session



-- MSG, UPDATE


type Msg
    = UUIDGenerated UUID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UUIDGenerated value ->
            ( { model | newLogId = Generated value }, Cmd.none )



-- VIEW


view : Model -> Html msg
view model =
    let
        routes =
            Route.routes
    in
    div
        []
        [ UI.viewLinkButton phrases.history routes.history
        , UI.viewLinkButton phrases.newLog4 (toNewLogUrl routes.editLog4 model)
        , UI.viewLinkButton phrases.newLog5 (toNewLogUrl routes.editLog5 model)
        ]


toNewLogUrl : String -> Model -> String
toNewLogUrl path model =
    case model.newLogId of
        Generating ->
            path

        Generated uuid ->
            path
                ++ "/"
                ++ UUID.toString uuid


phrases =
    { newLog4 = "新規作成(4人)"
    , newLog5 = "新規作成(5人)"
    , history = "今までの成績をみる"
    }
