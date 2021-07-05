module EditGame exposing (GameId, Model, Msg, initModel, toSession, update, view)

import Array exposing (Array)
import Html exposing (Html, table, td, text, textarea, th, tr)
import Html.Attributes exposing (class, disabled, maxlength, value)
import Session exposing (Session)


{-| TODO:

1.  model に状態として持つ -> view に表示(いまここ)

2.  編集 -> model 更新できるようにする

3.  Total ~ ゲーム代込み まで計算結果を表示できるようにする

4.  rate などのconfig を表示する(いったんは固定値でOK)

5.  rate などのconfig を変更できるようにする


## memo

  - 3人うちと4人うちと5人うちどうやって状態もつ？

-}
type alias Hogr =
    Array Int


type alias FourPlayersRound =
    { player1Point : Int
    , player2Point : Int
    , player3Point : Int
    , player4Point : Int
    }


type alias FourPlayers =
    { player1Name : String
    , player2Name : String
    , player3Name : String
    , player4Name : String
    }


type alias FivePlayersRound =
    { player1Point : Int
    , player2Point : Int
    , player3Point : Int
    , player4Point : Int
    , player5Point : Int
    }


type alias FivePlayers =
    { player1Name : String
    , player2Name : String
    , player3Name : String
    , player4Name : String
    , player5Name : String
    }


type alias GameConfig =
    { rate : Int
    , chipRate : Int
    , gameFee : Int
    }


type Model
    = FourPlayersGame
        { session : Session
        , gameId : GameId
        , gameConfig : GameConfig
        , players : FourPlayers
        , rounds : Array FourPlayersRound
        }
    | FivePlayersGame
        { session : Session
        , gameId : GameId
        , gameConfig : GameConfig
        , players : FivePlayers
        , rounds : Array FivePlayersRound
        }


{-| UUID
-}
type alias GameId =
    String


initGameInfo : GameConfig
initGameInfo =
    { rate = 0
    , chipRate = 0
    , gameFee = 0
    }


initPlayers : FourPlayers
initPlayers =
    { player1Name = "player1"
    , player2Name = "player2"
    , player3Name = "player3"
    , player4Name = "player4"
    }


initRounds : FourPlayersRound
initRounds =
    { player1Point = 0
    , player2Point = 0
    , player3Point = 0
    , player4Point = 0
    }


initModel : GameId -> Session -> Model
initModel gameId session =
    FourPlayersGame
        { session = session
        , gameId = gameId
        , gameConfig = initGameInfo
        , players =
            initPlayers
        , rounds =
            Array.repeat 4
                initRounds
        }


toSession : Model -> Session
toSession model =
    case model of
        FourPlayersGame model_ ->
            model_.session

        FivePlayersGame model_ ->
            model_.session



-- toViewConfig : Model -> ViewConfig
-- toViewConfig model =
--     case model of
--         FivePlayersGame gameInfo ->
--             let
--                 { gameConfig, players, rounds } =
--                     gameInfo
--             in
--             { gameConfig = gameConfig
--             , players = players
--             , rounds = rounds
--             }
--         FivePlayersGame gameInfo ->
--             let
--                 { gameConfig, players, rounds } =
--                     gameInfo
--             in
--             { gameConfig = gameConfig
--             , players = players
--             , rounds = rounds
--             }
-- UPDATE, MSG


type Msg
    = UpdatedPoint Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedPoint roundNumber point ->
            ( model, Cmd.none )



-- VIEW
-- type alias ViewConfig =
--     { gameConfig : GameConfig
--     , players : FourPlayers
--     , rounds : Array FourPlayersRound
--     }


view : Model -> Html msg
view model =
    let
        viewEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content ] [] ]

        viewNotEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content, disabled True ] [] ]

        viewEditableTh content =
            th [ class "editGame_th" ] [ textarea [ class "editGame_input", value content ] [] ]

        viewNotEditableTh content =
            th [ class "editGame_th" ] [ text content ]

        viewEditableTrTh property player1Name player2Name player3Name player4Name player5Name =
            tr [ class "editGame_tr" ]
                [ viewNotEditableTh property
                , viewEditableTh player1Name
                , viewEditableTh player2Name
                , viewEditableTh player3Name
                , viewEditableTh player4Name
                , viewEditableTh player5Name
                ]

        viewEditableTrTd roundNumber player1Point player2Point player3Point player4Point player5Point =
            tr [ class "editGame_tr" ]
                [ td [ class "editGame_td" ] [ text roundNumber ]
                , viewEditableTd player1Point
                , viewEditableTd player2Point
                , viewEditableTd player3Point
                , viewEditableTd player4Point
                , viewEditableTd player5Point
                ]

        viewNotEditableTrTd roundNumber player1Point player2Point player3Point player4Point player5Point =
            tr [ class "editGame_tr" ]
                [ td [ class "editGame_td_notEditable" ] [ text roundNumber ]
                , viewNotEditableTd player1Point
                , viewNotEditableTd player2Point
                , viewNotEditableTd player3Point
                , viewNotEditableTd player4Point
                , viewNotEditableTd player5Point
                ]
    in
    table
        [ class "editGame_table" ]
        [ viewEditableTrTh "" model.players
        , viewEditableTrTd "1" "12000" "12000" "120000" "12000" "12000"
        , viewEditableTrTd "2" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "3" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "4" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "5" "12000" "12000" "120000" "12000" "12000"
        , viewEditableTrTd "6" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "7" "12000" "12000" "12000" "12000" "12000"
        , viewEditableTrTd "8" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "Total" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "チップ" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "収支" "12000" "12000" "12000" "12000" "12000"
        , viewNotEditableTrTd "ゲーム代込" "12000" "12000" "12000" "12000" "12000"
        ]
