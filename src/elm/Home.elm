module Home exposing (init, update, view)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Random
import Route exposing (Route(..))
import Session exposing (Session)
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( initModel session, Random.generate UUIDGenerated UUID.generator )


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

        phrases =
            Route.phrases
    in
    div
        []
        [ viewButton phrases.history routes.history
        , viewButton phrases.editGame (toNewRoomUrl routes.editGame model)
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


type alias PathString =
    String


viewButton : String -> PathString -> Html msg
viewButton phrase pathString =
    div
        [ class "home_button_container" ]
        [ div
            [ class "home_button_primary" ]
            [ a [ href pathString ] [ text phrase ]
            ]
        ]
