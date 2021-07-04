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
type alias ThreePlayersRound =
    { player1Point : Int
    , player2Point : Int
    , player3Point : Int
    }


type alias ThreePlayers =
    { player1Name : String
    , player2Name : String
    , player3Name : String
    }


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


type alias GameInfo =
    { rate : Int
    , chipRate : Int
    , gameFee : Int
    }


type Model
    = ThreePlayersGame
        { session : Session
        , gameId : GameId
        , gameInfo : GameInfo
        , players : ThreePlayers
        , rounds : Array ThreePlayersRound
        }
    | FourPlayersGame
        { session : Session
        , gameId : GameId
        , gameInfo : GameInfo
        , players : FourPlayers
        , rounds : Array FourPlayersRound
        }
    | FivePlayersGame
        { session : Session
        , gameId : GameId
        , gameInfo : GameInfo
        , players : FivePlayers
        , rounds : Array FivePlayersRound
        }


{-| UUID
-}
type alias GameId =
    String


initGameInfo : GameInfo
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
        , gameInfo = initGameInfo
        , players =
            initPlayers
        , rounds =
            Array.repeat 4
                initRounds
        }


toSession : Model -> Session
toSession model =
    case model of
        ThreePlayersGame model_ ->
            model_.session

        FourPlayersGame model_ ->
            model_.session

        FivePlayersGame model_ ->
            model_.session



-- UPDATE, MSG


type Msg
    = UpdatedPoint Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedPoint roundNumber point ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html msg
view model =
    let
        viewEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content ] [] ]

        viewNotEditableTd content =
            td [ class "editGame_td" ] [ textarea [ class "editGame_input", maxlength 6, value content, disabled True ] [] ]

        viewTh content =
            th [ class "editGame_th" ] [ text content ]

        viewTrTh property player1Name player2Name player3Name player4Name player5Name =
            tr [ class "editGame_tr" ]
                [ viewTh property
                , viewTh player1Name
                , viewTh player2Name
                , viewTh player3Name
                , viewTh player4Name
                , viewTh player5Name
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
        [ viewTrTh "" "player1" "player2" "player3" "player4" "player5"
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
