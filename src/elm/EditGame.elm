module EditGame exposing
    ( GameId
    , Model
    , Msg
    , initModel
    , toSession
    , update
    , view
    )

{-| TODO:

1.  (done)model に状態として持つ -> view に表示

2.  編集 -> model 更新できるようにする

3.  (いまここ)Total ~ ゲーム代込み まで計算結果を表示できるようにする

4.  チップを model にもたせる & 編集できるようにする

5.  rate などのconfig を表示する(いったんは固定値でOK)

6.  rate などのconfig を変更できるようにする

-}

import Array exposing (Array)
import Html exposing (Html, input, table, td, text, th, tr)
import Html.Attributes exposing (class, pattern, value)
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
    , chips : Chips
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
    String


type alias Chips =
    Array Chip


type alias Chip =
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
    Array.initialize
        4
        -- 4
        (\_ -> Array.initialize 4 (always ""))


initChips : Chips
initChips =
    Array.initialize
        4
        (always "")


initModel : GameId -> Session -> Model
initModel gameId session =
    { session = session
    , gameId = gameId
    , gameConfig = initGameInfo
    , players = initPlayers
    , rounds = initRounds
    , chips = initChips
    }


toSession : Model -> Session
toSession model =
    model.session



-- MSG, UPDATE


type Msg
    = ChangedPlayerName Int String
    | ChangedPoint Int Int String
    | ChangedChip Int String


toIntRounds : Rounds -> Array (Array Int)
toIntRounds rounds =
    Array.map
        (\round ->
            Array.map
                (\point -> Maybe.withDefault 0 (String.toInt point))
                round
        )
        rounds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    Maybe.map
                        (updateRound point)
                        (Array.get gameNumber model.rounds)

                nextModel =
                    case maybeUpdatedRound of
                        Just updatedRound ->
                            { model | rounds = updatedRound }

                        Nothing ->
                            model
            in
            ( nextModel, Cmd.none )

        ChangedChip playerIndex chip ->
            let
                nextChips =
                    Array.set
                        playerIndex
                        chip
                        model.chips
            in
            ( { model | chips = nextChips }, Cmd.none )



-- VIEW


type alias ViewConfig =
    { gameConfig : GameConfig
    , players : Players
    , rounds : Rounds
    , chips : Chips
    }


view : ViewConfig -> Html Msg
view { gameConfig, players, rounds, chips } =
    table
        [ class "editGame_table" ]
        (viewInputPlayersRow "" players
            :: List.map
                (\( roundNumber, round ) -> viewInputRoundRow roundNumber round)
                (List.indexedMap Tuple.pair (Array.toList rounds))
            ++ [ viewInputChipsRow phrase.chip chips

               -- ++ [ viewComputedRow phrase.chip (Array.repeat 4 100)
               , viewComputedRow phrase.pointBalance (calculateTotalPoints rounds)
               , viewComputedRow phrase.balance (Array.repeat 4 100)
               , viewComputedRow phrase.totalBalance (Array.repeat 4 100)
               ]
        )


{-| プレイヤー名入力マス
-}
viewInputPlayerCell : Int -> String -> Html Msg
viewInputPlayerCell index content =
    th
        [ class "editGame_th" ]
        [ input
            [ class "editaGame_inputCellInput"
            , value content
            , onInput <| ChangedPlayerName index
            ]
            []
        ]


type alias InputPointCellConfig =
    { roundNumber : Int
    , playerIndex : Int
    , point : String
    }


{-| 点数入力マス
-}
viewInputPointCell : InputPointCellConfig -> Html Msg
viewInputPointCell { roundNumber, playerIndex, point } =
    td
        [ class "editGame_td" ]
        [ input
            [ class "editaGame_inputCellInput"
            , value point
            , onInput <| ChangedPoint roundNumber playerIndex
            , pattern "[0-9]*"
            ]
            []
        ]


type alias InputChipCellConfig =
    { playerIndex : Int
    , chip : String
    }


{-| チップ入力マス
-}
viewInputChipsCell : InputChipCellConfig -> Html Msg
viewInputChipsCell { playerIndex, chip } =
    td
        [ class "editGame_td" ]
        [ input
            [ class "editaGame_inputCellInput"
            , value chip
            , onInput <| ChangedChip playerIndex
            , pattern "[0-9]*"
            ]
            []
        ]


{-| 計算結果マス
-}
viewComputedCell : Int -> Html msg
viewComputedCell content =
    td
        [ class "editGame_computedCell" ]
        [ text <| String.fromInt content ]


{-| プレイヤー名入力行
-}
viewInputPlayersRow : String -> Players -> Html Msg
viewInputPlayersRow property players_ =
    tr [ class "editGame_tr" ]
        (th [ class "editGame_th" ] [ text "" ]
            :: List.indexedMap viewInputPlayerCell (Array.toList players_)
        )


{-| 点棒入力行
-}
viewInputRoundRow : Int -> Array String -> Html Msg
viewInputRoundRow title round_ =
    tr [ class "editGame_tr" ]
        (td [ class "editGame_gameNumberCell" ] [ text <| String.fromInt (title + 1) ]
            :: List.indexedMap
                (\index point ->
                    viewInputPointCell
                        { playerIndex = index, point = point, roundNumber = title }
                )
                (Array.toList round_)
        )


{-| チップ入力行
-}
viewInputChipsRow : String -> Chips -> Html Msg
viewInputChipsRow title chips =
    tr [ class "editGame_tr" ]
        (td [ class "editGame_title" ]
            [ text title ]
            :: List.indexedMap
                (\index chip ->
                    viewInputChipsCell
                        { chip = chip, playerIndex = index }
                )
                (Array.toList chips)
        )


{-| 計算結果行
-}
viewComputedRow : String -> Array Int -> Html msg
viewComputedRow roundNumber numbers =
    tr [ class "editGame_tr" ]
        (td [ class "editGame_title" ] [ text roundNumber ]
            :: (List.map viewComputedCell <| Array.toList numbers)
        )



-- Functions for view


{-| Rounds から計算した収支などのデータ
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
        (toIntRounds rounds)


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
            ( Just roundHead, Just reducedValueHead ) ->
                Array.append
                    (Array.initialize
                        1
                        (\_ -> roundHead + reducedValueHead)
                    )
                    (incrementPointByPlayer
                        roundTail
                        reducedValuetail
                    )

            _ ->
                reducedValue



-- Const


phrase : { pointBalance : String, chip : String, balance : String, totalBalance : String }
phrase =
    { pointBalance = "点棒収支"
    , chip = "チップ(枚数)"
    , balance = "収支"
    , totalBalance = "トータル収支"
    }
