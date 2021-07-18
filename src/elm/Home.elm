module Home exposing (Model, Msg, init, initModel, toSession, update, view)

import Html exposing (Html, div)
import Random
import Route exposing (Route(..))
import Session exposing (Session)
import UI
import UUID exposing (UUID)


type alias Model =
    { session : Session
    , newGameId : UUIDS
    }


type UUIDS
    = Generating
    | Generated UUID


initModel : Session -> Model
initModel session =
    { session = session, newGameId = Generating }


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
            ( { model | newGameId = Generated value }, Cmd.none )



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
        , UI.viewLinkButton phrases.newGame (toNewRoomUrl routes.editGame model)
        ]


toNewRoomUrl : String -> Model -> String
toNewRoomUrl path model =
    case model.newGameId of
        Generating ->
            path

        Generated uuid ->
            path
                ++ "/"
                ++ UUID.toString uuid


phrases =
    { newGame = "新規作成"
    , history = "今までの成績をみる"
    }
