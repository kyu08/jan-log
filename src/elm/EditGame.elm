module EditGame exposing (GameId, Model, Msg, initModel, toSession, toViewConfig, update, view)

import Array exposing (Array)
import Html exposing (Html, input, table, td, text, textarea, th, tr)
import Html.Attributes exposing (class, disabled, maxlength, type_, value)
import Session exposing (Session)


{-| TODO:

1.  (done)model に状態として持つ -> view に表示

2.  (いまここ)編集 -> model 更新できるようにする

3.  Total ~ ゲーム代込み まで計算結果を表示できるようにする

4.  rate などのconfig を表示する(いったんは固定値でOK)

5.  rate などのconfig を変更できるようにする

-}
type alias Points =
    Array Int


type alias Players =
    Array String


type alias GameConfig =
    { rate : Int
    , chipRate : Int
    , gameFee : Int
    }


type alias Rounds =
    Array Points


{-| 当初は

    type Model
        = FourPlayersGame Info
        | FivePlayersGame Info

のように実装していたが、分岐が増えて大変なので players, rounds をあえて Array で持つことにした。
詳しくはこちら <https://github.com/kyu08/jan-log/issues/15>

-}
type alias Model =
    { session : Session
    , gameId : GameId
    , gameConfig : GameConfig
    , players : Players
    , rounds : Rounds
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


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initPlayers : Players
initPlayers =
    Array.fromList [ "player1", "player2", "player3", "player4" ]


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initRounds : Rounds
initRounds =
    Array.repeat 4 <| Array.fromList [ 30000, 7000, 20000, 43000 ]


initModel : GameId -> Session -> Model
initModel gameId session =
    { session = session
    , gameId = gameId
    , gameConfig = initGameInfo
    , players = initPlayers
    , rounds = initRounds
    }


toSession : Model -> Session
toSession model =
    model.session


{-| TODO: これ撲滅したい
-}
toIntArray : Rounds -> Array (Array Int)
toIntArray rounds =
    rounds


type Msg
    = UpdatedPoint Int Int


toViewConfig : Model -> ViewConfig
toViewConfig { gameConfig, players, rounds } =
    { gameConfig = gameConfig
    , players = players
    , rounds = rounds
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedPoint roundNumber point ->
            ( model, Cmd.none )



-- VIEW


type alias ViewConfig =
    { gameConfig : GameConfig
    , players : Players
    , rounds : Rounds
    }



-- 2.  (いまここ)編集 -> model 更新できるようにする


view : ViewConfig -> Html msg
view { gameConfig, players, rounds } =
    let
        viewEditableTd content =
            td [ class "editGame_td" ] [ input [ class "editGame_input", type_ "number", maxlength 6, value <| String.fromInt content ] [] ]

        viewNotEditableTd content =
            td [ class "editGame_td" ] [ input [ class "editGame_input", type_ "number", maxlength 6, value <| String.fromInt content, disabled True ] [] ]

        viewEditableTh content =
            th [ class "editGame_th" ] [ input [ class "editGame_input", value content ] [] ]

        viewNotEditableTh content =
            th [ class "editGame_th" ] [ text content ]

        viewEditableTrTh : String -> Players -> Html msg
        viewEditableTrTh property players_ =
            tr [ class "editGame_tr" ]
                ([ viewNotEditableTh property ]
                    ++ List.map viewEditableTh
                        (Array.toList players_)
                )

        viewEditableTrTd roundNumber round_ =
            tr [ class "editGame_tr" ]
                ([ td [ class "editGame_td" ] [ text <| String.fromInt roundNumber ] ]
                    ++ List.map viewEditableTd (Array.toList round_)
                )

        viewNotEditableTrTd roundNumber numbers =
            tr [ class "editGame_tr" ]
                ([ td [ class "editGame_td_notEditable" ]
                    [ text roundNumber ]
                 ]
                    ++ (List.map viewNotEditableTd <|
                            Array.toList numbers
                       )
                )
    in
    table
        [ class "editGame_table" ]
        ([ viewEditableTrTh "" players ]
            ++ List.map
                (\( roundNumber, round ) -> viewEditableTrTd roundNumber round)
                (List.indexedMap Tuple.pair (Array.toList <| toIntArray rounds))
            ++ [ viewNotEditableTrTd phrase.pointBalance (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.chip (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.balance (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.totalBalance (Array.repeat 4 100)
               ]
        )


phrase =
    { pointBalance = "ポイント収支"
    , chip = "チップ"
    , balance = "収支"
    , totalBalance = "トータル収支"
    }
