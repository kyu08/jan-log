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
    { rate : Rate
    , chipRate : ChipRate
    , gameFee : GameFee
    }


type alias Rate =
    Int


type alias ChipRate =
    Int


type alias GameFee =
    Int


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


{-| rate : 収支 = 点数 \* n としたときの n
chipRate : チップ収支 = チップ枚数 \* m としたときの m
-}
initGameInfo : GameConfig
initGameInfo =
    { rate = 100
    , chipRate = 2
    , gameFee = 5000
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


toIntArray : Array String -> Array Int
toIntArray stringArray =
    Array.map
        (\s -> Maybe.withDefault 0 (String.toInt s))
        stringArray


toIntRounds : Rounds -> Array (Array Int)
toIntRounds rounds =
    Array.map
        (\round -> toIntArray round)
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
    let
        totalPoint =
            calculateTotalPoint rounds

        totalPointIncludeChip =
            calculateTotalPointIncludeChip gameConfig.chipRate totalPoint chips

        totalBalanceExcludeGameFee =
            calculateTotalBalanceExcludeGameFee gameConfig.rate totalPointIncludeChip
    in
    table
        [ class "editGame_table" ]
        (viewInputPlayersRow "" players
            :: List.map
                (\( roundNumber, round ) -> viewInputRoundRow roundNumber round)
                (List.indexedMap Tuple.pair (Array.toList rounds))
            ++ [ viewInputChipsRow phrase.chip chips
               , viewComputedRow phrase.pointBalance totalPoint
               , viewComputedRow phrase.pointBalanceIncludeChip totalPointIncludeChip
               , viewComputedRow phrase.balance totalBalanceExcludeGameFee
               , viewComputedRow phrase.totalBalance (calculateTotalBalanceIncludeGameFee gameConfig.gameFee totalBalanceExcludeGameFee)
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
            , pattern "[-]??[0-9]*"
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


calculateTotalPoint : Rounds -> Stats
calculateTotalPoint rounds =
    Array.foldl
        (calculateFromTwoArray (+))
        Array.empty
        (toIntRounds rounds)


calculateFromTwoArray : (Int -> Int -> Int) -> Array Int -> Array Int -> Array Int
calculateFromTwoArray calculator operand reducedValue =
    let
        operandTail =
            Array.slice 1 (Array.length operand) operand

        reducedValuetail =
            Array.slice 1 (Array.length reducedValue) reducedValue
    in
    if reducedValue == Array.empty then
        operand

    else
        case ( Array.get 0 operand, Array.get 0 reducedValue ) of
            ( Just operandHead, Just reducedValueHead ) ->
                Array.append
                    (Array.initialize 1 (\_ -> calculator operandHead reducedValueHead))
                    (calculateFromTwoArray
                        calculator
                        operandTail
                        reducedValuetail
                    )

            _ ->
                reducedValue


{-| incrementPointByPlayer のインターフェイスに合わせる形で Array(Array Int)にして渡しているが微妙な気もする
-}
calculateTotalPointIncludeChip : ChipRate -> Array Int -> Chips -> Array Int
calculateTotalPointIncludeChip chipRate totalPoints chips =
    Array.foldl
        (calculateFromTwoArray (\chip reducedValue -> chip * chipRate + reducedValue))
        totalPoints
        (Array.initialize 1 (\_ -> toIntArray chips))


calculateTotalBalanceExcludeGameFee : Rate -> Array Int -> Array Int
calculateTotalBalanceExcludeGameFee rate totalPointIncludeChip =
    Array.map (\point -> point * rate) totalPointIncludeChip


calculateTotalBalanceIncludeGameFee : GameFee -> Array Int -> Array Int
calculateTotalBalanceIncludeGameFee gameFee totalBalanceExcludeGameFee =
    Array.map (\point -> point - gameFee) totalBalanceExcludeGameFee



-- Const


phrase : { pointBalance : String, pointBalanceIncludeChip : String, chip : String, balance : String, totalBalance : String }
phrase =
    { pointBalance = "ポイント収支"
    , pointBalanceIncludeChip = "チップ込収支"
    , chip = "チップ(枚数)"
    , balance = "収支"
    , totalBalance = "場代込み収支"
    }
