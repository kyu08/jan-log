module EditGame exposing
    ( Model
    , Msg
    , initModel
    , toSession
    , update
    , view
    )

import Array exposing (Array)
import GameId exposing (GameId)
import Html exposing (Html, div, input, label, table, td, text, th, tr)
import Html.Attributes exposing (class, for, id, pattern, value)
import Html.Events exposing (onInput)
import Session exposing (Session)
import UI


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
type alias GameConfig =
    { rate : Rate
    , chipRate : ChipRate
    , gameFee : GameFee
    }


type alias Rate =
    String


type alias ChipRate =
    String


type alias GameFee =
    String


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
    { rate = "100"
    , chipRate = "2"
    , gameFee = "5000"
    }


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initPlayers : Players
initPlayers =
    Array.fromList [ "player1", "player2", "player3", "player4" ]


initRound : Round
initRound =
    Array.initialize 4 (always "")


roundInitializer : a -> Round
roundInitializer =
    \_ -> initRound


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initRounds : Rounds
initRounds =
    Array.initialize
        4
        roundInitializer


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
    | ChangedRate String
    | ChangedChipRate String
    | ChangedGameFee String
    | ClickedAddRowButton


toIntValue : String -> Int
toIntValue string =
    Maybe.withDefault 0 (String.toInt string)


toIntArray : Array String -> Array Int
toIntArray stringArray =
    Array.map
        toIntValue
        stringArray


toIntRounds : Rounds -> Array (Array Int)
toIntRounds rounds =
    Array.map
        (\round -> toIntArray round)
        rounds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rounds, players, gameConfig, chips } as m) =
    case msg of
        ChangedPlayerName index playerName ->
            ( { m | players = Array.set index playerName players }, Cmd.none )

        ChangedPoint gameNumber playerIndex point ->
            let
                updateRound point_ round =
                    Array.set gameNumber
                        (Array.set
                            playerIndex
                            point_
                            round
                        )
                        rounds

                maybeUpdatedRound =
                    Maybe.map
                        (updateRound point)
                        (Array.get gameNumber rounds)

                nextModel =
                    case maybeUpdatedRound of
                        Just updatedRound ->
                            { m | rounds = updatedRound }

                        Nothing ->
                            m
            in
            ( nextModel, Cmd.none )

        ChangedChip playerIndex chip ->
            ( { m | chips = Array.set playerIndex chip chips }, Cmd.none )

        ChangedRate inputValue ->
            ( { m | gameConfig = { gameConfig | rate = inputValue } }, Cmd.none )

        ChangedChipRate inputValue ->
            ( { m | gameConfig = { gameConfig | chipRate = inputValue } }, Cmd.none )

        ChangedGameFee inputValue ->
            ( { m | gameConfig = { gameConfig | gameFee = inputValue } }, Cmd.none )

        ClickedAddRowButton ->
            ( { m | rounds = Array.push initRound rounds }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "editGame_container" ]
        [ viewEditGameConfig model.gameConfig
        , viewEditGame
            model
        , viewAddRowButton
        ]


viewAddRowButton : Html Msg
viewAddRowButton =
    UI.viewButton { phrase = phrase.addRow, onClickMsg = ClickedAddRowButton }


{-| 対局情報編集UI
-}
viewEditGameConfig : GameConfig -> Html Msg
viewEditGameConfig { rate, chipRate, gameFee } =
    div [ class "editGame_gameConfigContainer" ]
        [ viewEditGameConfigForm phrase.editGameConfigRate rate ChangedRate
        , viewEditGameConfigForm phrase.editGameConfigChipRate chipRate ChangedChipRate
        , viewEditGameConfigForm phrase.editGameConfigGameFee gameFee ChangedGameFee
        ]


{-| 対局情報編集フォーム
-}
viewEditGameConfigForm : String -> String -> (String -> Msg) -> Html Msg
viewEditGameConfigForm labelText inputValue onInputMsg =
    div [ class "editGame_gameConfigForm" ]
        [ label [ class "editGame_gameConfigLabel", for labelText ] [ text labelText ]
        , input [ class "editGame_gameConfigInput", id labelText, value inputValue, onInput onInputMsg ] []
        ]


{-| 成績編集UI
-}
viewEditGame : Model -> Html Msg
viewEditGame { gameConfig, players, rounds, chips } =
    let
        totalPoint =
            calculateTotalPoint rounds

        totalPointIncludeChip =
            calculateTotalPointIncludeChip (toIntValue gameConfig.chipRate) totalPoint chips

        totalBalanceExcludeGameFee =
            calculateTotalBalanceExcludeGameFee (toIntValue gameConfig.rate) totalPointIncludeChip
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
               , viewComputedRow phrase.totalBalance (calculateTotalBalanceIncludeGameFee (toIntValue gameConfig.gameFee) totalBalanceExcludeGameFee)
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

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
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
calculateTotalPointIncludeChip : Int -> Array Int -> Chips -> Array Int
calculateTotalPointIncludeChip chipRate totalPoints chips =
    Array.foldl
        (calculateFromTwoArray (\chip reducedValue -> chip * chipRate + reducedValue))
        totalPoints
        (Array.initialize 1 (\_ -> toIntArray chips))


calculateTotalBalanceExcludeGameFee : Int -> Array Int -> Array Int
calculateTotalBalanceExcludeGameFee rate totalPointIncludeChip =
    Array.map (\point -> point * rate) totalPointIncludeChip


calculateTotalBalanceIncludeGameFee : Int -> Array Int -> Array Int
calculateTotalBalanceIncludeGameFee gameFee totalBalanceExcludeGameFee =
    Array.map (\point -> point - gameFee) totalBalanceExcludeGameFee



-- Const


phrase =
    { pointBalance = "ポイント収支"
    , pointBalanceIncludeChip = "チップ込収支"
    , chip = "チップ(枚数)"
    , balance = "収支"
    , totalBalance = "ゲーム代込み収支"
    , editGameConfigRate = "レート"
    , editGameConfigChipRate = "レート(チップ)"
    , editGameConfigGameFee = "ゲーム代"
    , addRow = "行を追加する"
    }
