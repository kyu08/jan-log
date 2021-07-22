port module EditLog exposing
    ( Model
    , Msg
    , initCmd
    , initModel
    , subscriptions
    , toSession
    , update
    , view
    )

import Array exposing (Array)
import Html exposing (Html, div, input, label, table, td, text, th, tr)
import Html.Attributes exposing (class, for, id, pattern, value)
import Html.Events exposing (onInput)
import LogDto exposing (LogDto4, RoundObj4)
import LogId exposing (LogId)
import Session exposing (Session)
import UI


{-| 当初は

    type Model
        = FourPlayersLog Info
        | FivePlayersLog Info

のように実装していたが、分岐が増えて大変なので players, rounds をあえて Array で持つことにした。
詳しくはこちら <https://github.com/kyu08/jan-log/issues/15>

-}
type alias Model =
    { session : Session
    , logId : LogId
    , logConfig : LogConfig
    , players : Players
    , rounds : Rounds
    , chips : Chips
    , editingRoundIndex : EditingRoundIndex
    }


{-| UUID
-}
type alias LogConfig =
    { rate : Rate
    , chipRate : ChipRate
    , gameFee : GameFee
    , rankPoint : ( String, String )
    , topBonus : TopBonus
    }


type EditingRoundIndex
    = None
    | Editing Int


type alias TopBonus =
    String


type alias RankPointFirtst =
    String


type alias RankPointSecond =
    String


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
initLogConfig : LogConfig
initLogConfig =
    { rate = "100"
    , chipRate = "2"
    , gameFee = "5000"
    , rankPoint = ( "10", "20" )
    , topBonus = "50"
    }


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initPlayers : Players
initPlayers =
    Array.fromList [ "player1", "player2", "player3", "player4" ]


initRound : Round
initRound =
    Array.initialize 4 (always "0")


roundInitializer : a -> Round
roundInitializer =
    \_ -> initRound


{-| TODO: Rounds.elm をつくってファクトリーメソッドをつくる
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


initModel : LogId -> Session -> Model
initModel logId session =
    { session = session
    , logId = logId
    , logConfig = initLogConfig
    , players = initPlayers
    , rounds = initRounds
    , chips = initChips
    , editingRoundIndex = None
    }


initCmd : LogId -> Cmd msg
initCmd logId =
    fetchLog logId


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
    | FetchedLog LogDto4
    | ClickedSaveButton
    | ChangedRankPointFirst String
    | ChangedRankPointSecond String
    | ChangedTopBonus String
    | ClickedEditRoundButton Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rounds, players, logConfig, chips, editingRoundIndex } as m) =
    case msg of
        ChangedPlayerName index playerName ->
            ( { m | players = Array.set index playerName players }, Cmd.none )

        ChangedPoint logNumber playerIndex point ->
            let
                updateRound point_ round =
                    Array.set logNumber
                        (Array.set
                            playerIndex
                            point_
                            round
                        )
                        rounds

                maybeUpdatedRound =
                    Maybe.map
                        (updateRound point)
                        (Array.get logNumber rounds)

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
            ( { m | logConfig = { logConfig | rate = inputValue } }, Cmd.none )

        ChangedChipRate inputValue ->
            ( { m | logConfig = { logConfig | chipRate = inputValue } }, Cmd.none )

        ChangedGameFee inputValue ->
            ( { m | logConfig = { logConfig | gameFee = inputValue } }, Cmd.none )

        ClickedAddRowButton ->
            ( { m | rounds = Array.push initRound rounds }, Cmd.none )

        FetchedLog dto4 ->
            ( dto4ToModel m dto4, Cmd.none )

        ClickedSaveButton ->
            ( m, updateLog <| toLogDto4 m )

        ChangedRankPointFirst rankpointFirst ->
            ( { m | logConfig = { logConfig | rankPoint = Tuple.mapFirst (\_ -> rankpointFirst) logConfig.rankPoint } }
            , Cmd.none
            )

        ChangedRankPointSecond rankpointSecond ->
            ( { m | logConfig = { logConfig | rankPoint = Tuple.mapSecond (\_ -> rankpointSecond) logConfig.rankPoint } }
            , Cmd.none
            )

        ChangedTopBonus topBonus ->
            ( { m | logConfig = { logConfig | topBonus = topBonus } }, Cmd.none )

        ClickedEditRoundButton index ->
            case editingRoundIndex of
                Editing editingRoundIndexValue ->
                    if editingRoundIndexValue == index then
                        ( { m | editingRoundIndex = None }, Cmd.none )

                    else
                        ( { m | editingRoundIndex = Editing index }, Cmd.none )

                None ->
                    ( { m | editingRoundIndex = Editing index }, Cmd.none )


dto4ToModel : Model -> LogDto4 -> Model
dto4ToModel model logDto4 =
    { model
        | logId = logDto4.logId
        , logConfig =
            { rate = String.fromInt logDto4.rate
            , chipRate = String.fromInt logDto4.chipRate
            , gameFee = String.fromInt logDto4.gameFee
            , rankPoint =
                Tuple.pair
                    (String.fromInt <| getArrayElement 0 logDto4.rankPoint)
                    (String.fromInt <| getArrayElement 1 logDto4.rankPoint)
            , topBonus = String.fromInt logDto4.topBonus
            }
        , players = logDto4.players
        , rounds = Array.map toStringRound4 logDto4.rounds
        , chips = toStringArray logDto4.chips
    }


toStringRound4 : RoundObj4 -> Round
toStringRound4 { data0, data1, data2, data3 } =
    Array.map String.fromInt <| Array.fromList [ data0, data1, data2, data3 ]


toLogDto4 : Model -> LogDto4
toLogDto4 { logId, logConfig, players, rounds, chips } =
    { logId = logId
    , gameFee = toIntValue logConfig.gameFee
    , rate = toIntValue logConfig.rate
    , chipRate = toIntValue logConfig.chipRate
    , players = players
    , rounds = Array.map toRoundObj4 rounds
    , chips = toIntArray chips
    , rankPoint = Array.fromList [ toIntValue <| Tuple.first logConfig.rankPoint, toIntValue <| Tuple.second logConfig.rankPoint ]
    , topBonus = toIntValue logConfig.topBonus
    }


toRoundObj4 : Round -> RoundObj4
toRoundObj4 round =
    let
        roundInt =
            toIntArray round
    in
    { data0 = getArrayElement 0 roundInt
    , data1 = getArrayElement 1 roundInt
    , data2 = getArrayElement 2 roundInt
    , data3 = getArrayElement 3 roundInt
    }


getArrayElement : Int -> Array Int -> Int
getArrayElement index array =
    Maybe.withDefault 0 <|
        Array.get
            index
            array


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


toStringArray : Array Int -> Array String
toStringArray arrayInt =
    Array.map String.fromInt arrayInt



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "editLog_container" ]
        [ viewEditLogConfig model.logConfig
        , viewEditLog model
        , UI.viewButton { phrase = phrase.addRow, onClickMsg = ClickedAddRowButton, size = UI.Default }
        , UI.viewButton { phrase = "保存する", onClickMsg = ClickedSaveButton, size = UI.Default }
        ]


{-| 対局情報編集UI
-}
viewEditLogConfig : LogConfig -> Html Msg
viewEditLogConfig { rate, chipRate, gameFee, rankPoint, topBonus } =
    div [ class "editLog_logConfigContainer" ]
        [ viewEditLogConfigForm phrase.editLogConfigRate rate ChangedRate
        , viewEditLogConfigForm phrase.editLogConfigChipRate chipRate ChangedChipRate
        , viewEditLogConfigForm phrase.editLogConfigGameFee gameFee ChangedGameFee
        , viewEditLogConfigForm phrase.editLogConfigTopBonus topBonus ChangedTopBonus
        , viewEditLogConfigForm phrase.editLogConfigRankPointFirst (Tuple.first rankPoint) ChangedRankPointFirst
        , viewEditLogConfigForm phrase.editLogConfigRankPointSecond (Tuple.second rankPoint) ChangedRankPointSecond
        ]


{-| 対局情報編集フォーム
-}
viewEditLogConfigForm : String -> String -> (String -> Msg) -> Html Msg
viewEditLogConfigForm labelText inputValue onInputMsg =
    div [ class "editLog_logConfigForm" ]
        [ label [ class "editLog_logConfigLabel", for labelText ] [ text labelText ]
        , input [ class "editLog_logConfigInput", id labelText, value inputValue, onInput onInputMsg ] []
        ]


{-| 成績編集UI
-}
viewEditLog : Model -> Html Msg
viewEditLog { logConfig, players, rounds, chips, editingRoundIndex } =
    let
        totalPoint =
            calculateTotalPoint rounds

        totalPointIncludeChip =
            calculateTotalPointIncludeChip (toIntValue logConfig.chipRate) totalPoint chips

        totalBalanceExcludeGameFee =
            calculateTotalBalanceExcludeGameFee (toIntValue logConfig.rate) totalPointIncludeChip

        totalBalanceIncludeGameFee =
            calculateTotalBalanceIncludeGameFee (toIntValue logConfig.gameFee) totalBalanceExcludeGameFee
    in
    table
        [ class "editLog_table" ]
        (viewInputPlayersRow players
            :: (Array.toList <|
                    Array.indexedMap
                        (\roundNumber round -> viewInputRoundRow roundNumber editingRoundIndex round)
                        rounds
               )
            ++ [ viewInputChipsRow phrase.chip chips
               , viewCalculatedRow phrase.pointBalance totalPoint
               , viewCalculatedRow phrase.pointBalanceIncludeChip totalPointIncludeChip
               , viewCalculatedRow phrase.balance totalBalanceExcludeGameFee
               , viewCalculatedRow phrase.totalBalance totalBalanceIncludeGameFee
               ]
        )


{-| プレイヤー名入力行
-}
viewInputPlayersRow : Players -> Html Msg
viewInputPlayersRow players =
    tr [ class "editLog_tr" ]
        (th [ class "editLog_th" ] [ text "" ]
            :: List.indexedMap viewInputPlayerCell (Array.toList players)
        )


{-| 点棒入力行
-}
viewInputRoundRow : Int -> EditingRoundIndex -> Array String -> Html Msg
viewInputRoundRow roundNumber editingRoundIndex round =
    let
        roundInputArea =
            case editingRoundIndex of
                None ->
                    List.indexedMap
                        (\index_ point -> viewShowPointCell point)
                        (Array.toList round)

                Editing index ->
                    if roundNumber == index then
                        List.indexedMap
                            (\index_ point -> viewInputPointCell roundNumber index_ point)
                            (Array.toList round)

                    else
                        List.indexedMap
                            (\index_ point -> viewShowPointCell point)
                            (Array.toList round)
    in
    tr [ class "editLog_tr" ]
        (td
            [ class "editLog_logNumberCell" ]
            [ viewInputPointButton roundNumber, text <| String.fromInt (roundNumber + 1) ]
            :: roundInputArea
        )


{-| チップ入力行
-}
viewInputChipsRow : String -> Chips -> Html Msg
viewInputChipsRow title chips =
    tr [ class "editLog_tr" ]
        (td [ class "editLog_title" ]
            [ text title ]
            :: List.indexedMap
                (\index chip -> viewInputChipsCell index chip)
                (Array.toList chips)
        )


{-| 計算結果行
-}
viewCalculatedRow : String -> Array Int -> Html msg
viewCalculatedRow roundNumber calculatedValues =
    tr [ class "editLog_tr" ]
        (td [ class "editLog_title" ] [ text roundNumber ]
            :: (List.map viewCalculatedCell <| Array.toList calculatedValues)
        )


{-| プレイヤー名入力マス
-}
viewInputPlayerCell : Int -> String -> Html Msg
viewInputPlayerCell playerIndex playerName =
    th
        [ class "editLog_th" ]
        [ input
            [ class "editLog_inputCellInput", value playerName, onInput <| ChangedPlayerName playerIndex ]
            []
        ]


{-| 点数入力マス
-}
viewInputPointCell : Int -> Int -> Point -> Html Msg
viewInputPointCell roundNumber playerIndex point =
    td
        [ class "editLog_td" ]
        [ input
            [ class "editLog_inputCellInput"
            , value point
            , onInput <| ChangedPoint roundNumber playerIndex

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
            ]
            []
        ]


{-| 点数表示マス
-}
viewShowPointCell : Point -> Html Msg
viewShowPointCell point =
    td
        [ class "editLog_calculatedCell" ]
        [ text point ]


{-| チップ入力マス
-}
viewInputChipsCell : Int -> String -> Html Msg
viewInputChipsCell playerIndex chip =
    td
        [ class "editLog_td" ]
        [ input
            [ class "editLog_inputCellInput"
            , value chip
            , onInput <| ChangedChip playerIndex

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
            ]
            []
        ]


{-| 計算結果マス
-}
viewCalculatedCell : Int -> Html msg
viewCalculatedCell calculatedValue =
    td
        [ class "editLog_calculatedCell" ]
        [ text <| String.fromInt calculatedValue ]


viewInputPointButton : Int -> Html Msg
viewInputPointButton index =
    UI.viewButton
        { phrase = phrase.inputPoint
        , onClickMsg = ClickedEditRoundButton index
        , size = UI.Mini
        }



-- Functions for view


{-| Rounds から計算した収支などのデータ
-}
type alias Stats =
    Array Stat


type alias Stat =
    Int


{-| ポイント収支を計算する
-}
calculateTotalPoint : Rounds -> Stats
calculateTotalPoint rounds =
    Array.foldl
        (calculateFrom2Arrays (+))
        Array.empty
        (toIntRounds rounds)


{-| 2つの Array を元に計算を行う
-}
calculateFrom2Arrays : (Int -> Int -> Int) -> Array Int -> Array Int -> Array Int
calculateFrom2Arrays calculator operand reducedValue =
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
                    (calculateFrom2Arrays
                        calculator
                        operandTail
                        reducedValuetail
                    )

            _ ->
                reducedValue


{-| チップ込み収支を計算する
incrementPointByPlayer のインターフェイスに合わせる形で Array(Array Int)にして渡しているが微妙な気もする
-}
calculateTotalPointIncludeChip : Int -> Array Int -> Chips -> Array Int
calculateTotalPointIncludeChip chipRate totalPoints chips =
    Array.foldl
        (calculateFrom2Arrays (\chip reducedValue -> chip * chipRate + reducedValue))
        totalPoints
        (Array.initialize 1 (\_ -> toIntArray chips))


{-| ゲーム代を含まない収支を計算する
-}
calculateTotalBalanceExcludeGameFee : Int -> Array Int -> Array Int
calculateTotalBalanceExcludeGameFee rate totalPointIncludeChip =
    Array.map (\point -> point * rate) totalPointIncludeChip


{-| ゲーム代込み収支を計算する
-}
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
    , editLogConfigRate = "レート"
    , editLogConfigChipRate = "レート(チップ)"
    , editLogConfigGameFee = "ゲーム代"
    , editLogConfigTopBonus = "オカ"
    , editLogConfigRankPointFirst = "ウマ(2, 3着)"
    , editLogConfigRankPointSecond = "ウマ(1, 4着)"
    , addRow = "行を追加する"
    , inputPoint = "🖋"
    }



-- Subs


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ fetchedLog FetchedLog ]



-- Ports


port fetchLog : String -> Cmd msg


port updateLog : LogDto4 -> Cmd msg


port fetchedLog : (LogDto4 -> msg) -> Sub msg
