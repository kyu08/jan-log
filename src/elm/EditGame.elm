module EditGame exposing (GameId, Model, Msg, initModel, toSession, toViewConfig, update, view)

{-| TODO:

1.  (done)model に状態として持つ -> view に表示

2.  編集 -> model 更新できるようにする

3.  (いまここ)Total ~ ゲーム代込み まで計算結果を表示できるようにする

4.  rate などのconfig を表示する(いったんは固定値でOK)

5.  rate などのconfig を変更できるようにする

-}

import Array exposing (Array, foldl)
import Html exposing (Html, div, input, table, td, text, th, tr)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onInput)
import Session exposing (Session)


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


type alias GameConfig =
    { rate : Int
    , chipRate : Int
    , gameFee : Int
    }


type alias Players =
    Array String


type alias Rounds =
    Array Round


{-| 半荘データ
-}
type alias Round =
    Array Point


type alias Point =
    Int


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
    Array.initialize
        4
        -- 4
        (\_ -> Array.initialize 4 (always 0))


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


type Msg
    = UpdatedPoint Int Int
    | ChangedPlayerName Int String
    | ChangedPoint Int Int String


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

        ChangedPlayerName index playerName ->
            let
                players =
                    Array.set index playerName model.players
            in
            ( { model | players = players }, Cmd.none )

        ChangedPoint gameNumber playerIndex point ->
            let
                updateRound point_ round =
                    Array.set gameNumber
                        (Array.set
                            playerIndex
                            point_
                            round
                        )
                        model.rounds

                maybeUpdatedRound =
                    Maybe.map2
                        updateRound
                        (String.toInt point)
                        (Array.get gameNumber model.rounds)

                nextModel =
                    case maybeUpdatedRound of
                        Just updatedRound ->
                            { model | rounds = updatedRound }

                        Nothing ->
                            model
            in
            ( nextModel, Cmd.none )



-- VIEW


type alias ViewConfig =
    { gameConfig : GameConfig
    , players : Players
    , rounds : Rounds
    }


view : ViewConfig -> Html Msg
view { gameConfig, players, rounds } =
    table
        [ class "editGame_table" ]
        (viewEditableTrTh "" players
            :: List.map
                (\( roundNumber, round ) -> viewEditableTrTd roundNumber round)
                (List.indexedMap Tuple.pair (Array.toList rounds))
            ++ [ viewNotEditableTrTd phrase.pointBalance (calculateTotalPoints rounds)

               -- ++ [ viewNotEditableTrTd phrase.pointBalance (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.chip (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.balance (Array.repeat 4 100)
               , viewNotEditableTrTd phrase.totalBalance (Array.repeat 4 100)
               ]
        )


{-| 左上の空白マス
-}
viewNotEditableTh : String -> Html msg
viewNotEditableTh content =
    th [ class "editGame_th" ] [ text content ]


{-| プレイヤー名入力マス
-}
viewEditableTh : Int -> String -> Html Msg
viewEditableTh index content =
    th
        [ class "editGame_th" ]
        [ input [ class "editGame_input", value content, onInput <| ChangedPlayerName index ] [] ]


type alias EditableTdConfig =
    { roundNumber : Int
    , playerIndex : Int
    , point : Int
    }


{-| 点数入力マス
-}
viewEditableTd : EditableTdConfig -> Html Msg
viewEditableTd { roundNumber, playerIndex, point } =
    td
        [ class "editGame_td" ]
        [ input
            [ class "editGame_input"
            , type_ "number"
            , value <| String.fromInt point
            , onInput <| ChangedPoint roundNumber playerIndex
            ]
            []
        ]


{-| 計算結果マス
-}
viewNotEditableTd : Int -> Html msg
viewNotEditableTd content =
    td
        [ class "editGame_td" ]
        [ input
            [ class "editGame_input", type_ "number", value <| String.fromInt content, disabled True ]
            []
        ]


{-| プレイヤー名入力行
-}
viewEditableTrTh : String -> Players -> Html Msg
viewEditableTrTh property players_ =
    tr [ class "editGame_tr" ]
        (viewNotEditableTh property
            :: List.indexedMap viewEditableTh (Array.toList players_)
        )


{-| 点数入力行
-}
viewEditableTrTd : Int -> Array Int -> Html Msg
viewEditableTrTd roundNumber round_ =
    tr [ class "editGame_tr" ]
        (td [ class "editGame_td" ] [ text <| String.fromInt (roundNumber + 1) ]
            :: List.indexedMap
                (\index point ->
                    viewEditableTd
                        { playerIndex = index, point = point, roundNumber = roundNumber }
                )
                (Array.toList round_)
        )


{-| 計算結果行
-}
viewNotEditableTrTd : String -> Array Int -> Html msg
viewNotEditableTrTd roundNumber numbers =
    tr [ class "editGame_tr" ]
        (td [ class "editGame_td_notEditable" ] [ text roundNumber ]
            :: (List.map viewNotEditableTd <| Array.toList numbers)
        )



-- Functions for view


{-| Rounds から計算した収支などのデータ
TODO: 命名はもう少し考えよう
-}
type alias Stats =
    Array Stat


type alias Stat =
    Int


calculateTotalPoints : Rounds -> Stats
calculateTotalPoints rounds =
    Array.foldl
        incrementPointByPlayer
        Array.empty
        rounds


incrementPointByPlayer : Array Int -> Array Int -> Array Int
incrementPointByPlayer round reducedValue =
    let
        maybeRoundHead =
            Array.get 0 round

        roundTail =
            Array.slice 1 (Array.length round) round

        maybeReducedValueHead =
            Array.get 0 reducedValue

        reducedValuetail =
            Array.slice 1 (Array.length reducedValue) reducedValue
    in
    if reducedValue == Array.empty then
        round

    else
        case ( maybeRoundHead, maybeReducedValueHead ) of
            ( Just head1, Just head2 ) ->
                Array.append
                    (Array.initialize
                        1
                        (\_ -> head1 + head2)
                    )
                    (incrementPointByPlayer
                        roundTail
                        reducedValuetail
                    )

            _ ->
                reducedValue



-- ( Nothing, Nothing ) ->
--     reducedValue
-- ( Just head1, Nothing ) ->
--     incrementPointByPlayer
--         tail
--         (Array.append
--             (Array.initialize 1 (\_ -> head1))
--             reducedValue
--         )
-- ( Nothing, Just head2 ) ->
--     incrementPointByPlayer
--         Array.empty
--         (Array.append
--             (Array.initialize 1 (\_ -> head2))
--             reducedValue
--         )
-- Const


phrase : { pointBalance : String, chip : String, balance : String, totalBalance : String }
phrase =
    { pointBalance = "ポイント収支"
    , chip = "チップ"
    , balance = "収支"
    , totalBalance = "トータル収支"
    }
