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
import Html exposing (Html, div, input, label, p, table, td, text, th, tr)
import Html.Attributes exposing (class, for, id, value)
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
    , isOpenedConfigArea : Bool
    , isOpenedHowToUseArea : Bool
    , editingRoundIndex : EditingRoundIndex
    }


type alias LogConfig =
    { rate : Rate
    , chipRate : ChipRate
    , gameFee : GameFee
    , rankPoint : RankPoint

    -- 何万点持ちか
    , havePoint : Point

    -- 何万点返しか
    , returnPoint : Point
    }


type alias RankPoint =
    ( String, String )


type EditingRoundIndex
    = None
    | Editing Int


type alias Rate =
    String


type alias ChipRate =
    String


type alias GameFee =
    String


type alias Players =
    Array Player


type alias Player =
    String


type alias Rounds =
    Array Round


{-| 半荘データ
同点の場合は起家を入力して順位点を確定する
chicha: PlayerIndex
-}
type alias Round =
    { points : Array Point
    , chicha : Maybe Int
    }


type alias IntRound =
    { chicha : Maybe Int
    , points : Array Int
    }


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
    , havePoint = "30"
    , returnPoint = "25"
    }


{-| TODO: Players.elm をつくってファクトリーメソッドをつくる
-}
initPlayers : Players
initPlayers =
    Array.fromList [ "player1", "player2", "player3", "player4" ]


initRound4 : Round
initRound4 =
    { points = Array.initialize 4 (always "")
    , chicha = Nothing
    }


roundInitializer : a -> Round
roundInitializer =
    \_ -> initRound4


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
    , isOpenedConfigArea = False
    , isOpenedHowToUseArea = False
    , editingRoundIndex = None
    }


initCmd : LogId -> Cmd msg
initCmd logId =
    Cmd.batch
        [ fetchLog logId
        , listenLog logId
        ]


toSession : Model -> Session
toSession model =
    model.session



-- MSG, UPDATE


type Msg
    = ChangedPlayerName Int String
    | ChangedPoint Int Int Point
    | ChangedChip Int String
    | ChangedRate String
    | ChangedChipRate String
    | ChangedGameFee String
    | ClickedAddRowButton
    | ClickedToggleConfigButton
    | ClickedHowToUseButton
    | FetchedLog LogDto4
    | ListenedLog LogDto4
    | ChangedRankPointFirst String
    | ChangedRankPointSecond String
    | ChangedHavePoint String
    | ChangedReturnPoint String
    | ClickedEditRoundButton Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rounds, players, logConfig, chips, isOpenedConfigArea, editingRoundIndex, isOpenedHowToUseArea } as m) =
    case msg of
        ChangedPlayerName index playerName ->
            let
                nextModel =
                    { m | players = Array.set index playerName players }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedPoint logNumber playerIndex point ->
            let
                updateRound point_ round_ =
                    Array.set logNumber
                        { round_
                            | points =
                                Array.set
                                    playerIndex
                                    point_
                                    round_.points
                        }
                        rounds

                maybeUpdatedRounds =
                    Maybe.map
                        (updateRound point)
                        (Array.get logNumber rounds)

                nextModel =
                    case maybeUpdatedRounds of
                        Just updatedRound ->
                            { m | rounds = updatedRound }

                        Nothing ->
                            m
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedChip playerIndex chip ->
            let
                nextModel =
                    { m | chips = Array.set playerIndex chip chips }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedRate inputValue ->
            let
                nextModel =
                    { m | logConfig = { logConfig | rate = inputValue } }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedChipRate inputValue ->
            let
                nextModel =
                    { m | logConfig = { logConfig | chipRate = inputValue } }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedGameFee inputValue ->
            let
                nextModel =
                    { m | logConfig = { logConfig | gameFee = inputValue } }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ClickedAddRowButton ->
            ( { m | rounds = Array.push initRound4 rounds }, Cmd.none )

        ClickedToggleConfigButton ->
            ( { m | isOpenedConfigArea = not isOpenedConfigArea }, Cmd.none )

        ClickedHowToUseButton ->
            ( { m | isOpenedHowToUseArea = not isOpenedHowToUseArea }, Cmd.none )

        FetchedLog dto4 ->
            ( dto4ToModel m dto4, Cmd.none )

        ListenedLog dto4 ->
            ( dto4ToModel m dto4, Cmd.none )

        ChangedRankPointFirst rankpointFirst ->
            let
                nextModel =
                    { m
                        | logConfig =
                            { logConfig | rankPoint = Tuple.mapFirst (\_ -> rankpointFirst) logConfig.rankPoint }
                    }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedRankPointSecond rankpointSecond ->
            let
                nextModel =
                    { m
                        | logConfig = { logConfig | rankPoint = Tuple.mapSecond (\_ -> rankpointSecond) logConfig.rankPoint }
                    }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedReturnPoint returnPoint ->
            let
                nextModel =
                    { m | logConfig = { logConfig | returnPoint = returnPoint } }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedHavePoint havePoint ->
            let
                nextModel =
                    { m | logConfig = { logConfig | havePoint = havePoint } }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        -- ここで同点判定をする
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
            , havePoint = String.fromInt logDto4.havePoint
            , returnPoint = String.fromInt logDto4.returnPoint
            }
        , players = logDto4.players
        , rounds = Array.map toStringRound4 logDto4.rounds
        , chips = toStringArray logDto4.chips
    }


toStringRound : IntRound -> Round
toStringRound intRound =
    { chicha = intRound.chicha
    , points = Array.map String.fromInt intRound.points
    }


toStringRound4 : RoundObj4 -> Round
toStringRound4 { points, chicha } =
    let
        stringFromInt int =
            if int == 0 then
                ""

            else
                String.fromInt int
    in
    { chicha = chicha
    , points =
        Array.map stringFromInt <|
            Array.fromList
                [ points.data0
                , points.data1
                , points.data2
                , points.data3
                ]
    }


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
    , havePoint = toIntValue logConfig.havePoint
    , returnPoint = toIntValue logConfig.returnPoint
    }


toRoundObj4 : Round -> RoundObj4
toRoundObj4 { chicha, points } =
    let
        pointsInt =
            toIntArray points
    in
    { chicha = chicha
    , points =
        { data0 = getArrayElement 0 pointsInt
        , data1 = getArrayElement 1 pointsInt
        , data2 = getArrayElement 2 pointsInt
        , data3 = getArrayElement 3 pointsInt
        }
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


toIntRound : Round -> IntRound
toIntRound { chicha, points } =
    { chicha = chicha
    , points = toIntArray points
    }


toStringArray : Array Int -> Array String
toStringArray arrayInt =
    Array.map String.fromInt arrayInt


toIntTuple : ( String, String ) -> ( Int, Int )
toIntTuple stringTuple =
    Tuple.mapBoth
        toIntValue
        toIntValue
        stringTuple



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewPointInputModal_ =
            case model.editingRoundIndex of
                None ->
                    UI.viewBlank

                Editing index ->
                    case Array.get index model.rounds of
                        Nothing ->
                            UI.viewBlank

                        Just round ->
                            viewPointInputModal model.players round
    in
    div [ class "editLog_container" ]
        [ viewEditLog model
        , UI.viewButton { phrase = phrase.addRow, onClickMsg = ClickedAddRowButton, size = UI.Default }
        , viewToggleHowToUseButton model.isOpenedHowToUseArea
        , viewHowToUse model.isOpenedHowToUseArea
        , viewToggleLogConfigAreaBottun
            model.isOpenedConfigArea
        , viewEditLogConfig
            model.logConfig
            model.isOpenedConfigArea
        , viewPointInputModal_
        ]


viewHowToUse : Bool -> Html msg
viewHowToUse isOpened =
    if isOpened then
        div [ class "editLog_howToUseContainer" ]
            [ p [] [ text "1. 半荘が終了したら 🖋 をタップして素点を入力する" ]
            , p [] [ text "2. 素点の100の位を五捨六入して1000で割った値を入力する" ]
            , p [] [ text "3. 同点の場合は起家を入力する" ]
            , p [] [ text "4. 入力が完了したら再度🖋をタップして入力を終了する。(順位点を加味した値が表示されます)" ]
            ]

    else
        UI.viewBlank


viewToggleHowToUseButton : Bool -> Html Msg
viewToggleHowToUseButton isOpened =
    if isOpened then
        UI.viewButton
            { phrase = phrase.closeHowToUseArea
            , onClickMsg = ClickedHowToUseButton
            , size = UI.Default
            }

    else
        UI.viewButton
            { phrase = phrase.openHowToUseArea
            , onClickMsg = ClickedHowToUseButton
            , size = UI.Default
            }


viewToggleLogConfigAreaBottun : Bool -> Html Msg
viewToggleLogConfigAreaBottun isOpened =
    if isOpened then
        UI.viewButton
            { phrase = phrase.closeEditLogConfigArea
            , onClickMsg = ClickedToggleConfigButton
            , size = UI.Default
            }

    else
        UI.viewButton
            { phrase = phrase.openEditLogConfigArea
            , onClickMsg = ClickedToggleConfigButton
            , size = UI.Default
            }


{-| 対局情報編集UI
-}
viewEditLogConfig : LogConfig -> Bool -> Html Msg
viewEditLogConfig { rate, chipRate, gameFee, rankPoint, havePoint, returnPoint } isOpened =
    if isOpened then
        div
            [ class "editLog_logConfigContainer" ]
            [ viewEditLogConfigForm phrase.editLogConfigRate rate ChangedRate
            , viewEditLogConfigForm phrase.editLogConfigChipRate chipRate ChangedChipRate
            , viewEditLogConfigForm phrase.editLogConfigGameFee gameFee ChangedGameFee
            , viewEditLogConfigForm phrase.editLogConfigHavePoint havePoint ChangedHavePoint
            , viewEditLogConfigForm phrase.editLogConfigReturnPoint returnPoint ChangedReturnPoint
            , viewEditLogConfigForm phrase.editLogConfigRankPointFirst (Tuple.first rankPoint) ChangedRankPointFirst
            , viewEditLogConfigForm phrase.editLogConfigRankPointSecond (Tuple.second rankPoint) ChangedRankPointSecond
            ]

    else
        UI.viewBlank


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
            rounds
                |> Array.map
                    (\round ->
                        if not <| isDefaultRound round then
                            calculateRoundFromRawPoint
                                { rankPoint = toIntTuple logConfig.rankPoint
                                , round = toIntRound round
                                , havePoint = toIntValue logConfig.havePoint
                                , returnPoint = toIntValue logConfig.returnPoint
                                }

                        else
                            toIntRound round
                    )
                |> Array.map .points
                |> calculateTotalPoint

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
                        (\roundNumber round ->
                            viewInputRoundRow
                                { roundNumber = roundNumber
                                , editingRoundIndex = editingRoundIndex
                                , round = round
                                , rankPoint = logConfig.rankPoint
                                , havePoint = logConfig.havePoint
                                , returnPoint = logConfig.returnPoint
                                }
                        )
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


type alias ViewInputRoundRowConfig =
    { roundNumber : Int
    , editingRoundIndex : EditingRoundIndex
    , round : Round
    , rankPoint : RankPoint
    , havePoint : Point
    , returnPoint : Point
    }


isDefaultRound : Round -> Bool
isDefaultRound { points } =
    (points == initRound4.points)
        || (points == Array.initialize 4 (always "0"))


{-| 点棒入力行
-}
viewInputRoundRow : ViewInputRoundRowConfig -> Html Msg
viewInputRoundRow { roundNumber, editingRoundIndex, round, rankPoint, havePoint, returnPoint } =
    let
        points =
            calculateRoundFromRawPoint
                { rankPoint = toIntTuple rankPoint
                , round = toIntRound round
                , havePoint = toIntValue havePoint
                , returnPoint = toIntValue returnPoint
                }
                |> toStringRound
                >> .points

        viewShowPointCell_ =
            if isDefaultRound round then
                Array.map
                    viewShowPointCell
                    round.points
                    |> Array.toList

            else
                points
                    |> Array.map viewShowPointCell
                    |> Array.toList

        roundInputArea =
            case editingRoundIndex of
                None ->
                    viewShowPointCell_

                Editing index ->
                    if roundNumber == index then
                        List.indexedMap
                            (\index_ point -> viewInputPointCell roundNumber index_ point)
                            (Array.toList round.points)

                    else
                        viewShowPointCell_
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
ここでウマオカとかを計算して表示する
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


viewPointInputModal : Players -> Round -> Html Msg
viewPointInputModal players round =
    UI.viewModal <| text "test"



-- Functions for view


{-| Rounds から計算した収支などのデータ
-}
type alias Stats =
    Array Stat


type alias Stat =
    Int


{-| ポイント収支を計算する
-}
calculateTotalPoint : Array (Array Int) -> Stats
calculateTotalPoint rounds =
    Array.foldl
        (calculateFrom2Arrays (+))
        Array.empty
        rounds


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


type alias CalculateRoundFromRawPointConfig =
    { round : IntRound
    , rankPoint : ( Int, Int )
    , havePoint : Int
    , returnPoint : Int
    }


{-| 入力されたポイントをもとに順位点を加算したポイントを返す関数
トビを考慮するために1着のポイント計算方法を - (2~4着のトータルポイント) としている
TODO: 同点の時は起家をユーザーにきく
TODO: 同点の場合は起家を考慮して順位点を決定する
TODO: トビは現状の実装だと場外で(チップなどで)やりとりするしかないので↑の計算方法をやめる。
-}
calculateRoundFromRawPoint : CalculateRoundFromRawPointConfig -> IntRound
calculateRoundFromRawPoint { round, rankPoint, havePoint, returnPoint } =
    let
        rankPointArray =
            [ Tuple.second rankPoint
            , Tuple.first rankPoint
            , negate <| Tuple.first rankPoint
            , negate <| Tuple.second rankPoint
            ]

        returnedRound =
            Array.indexedMap
                (\index point -> ( index, point - returnPoint ))
                round.points

        sortedRound =
            List.reverse <|
                List.sortBy
                    Tuple.second
                    (Array.toList returnedRound)

        rankPointedRound =
            List.map2
                (\rankPoint_ ( rank, ( index, point ) ) ->
                    ( rank, ( index, point + rankPoint_ ) )
                )
                rankPointArray
                (List.indexedMap (\rank roundWithIndex -> ( rank, roundWithIndex )) sortedRound)

        totalPointsWithout1st =
            List.foldl
                (\( rank, ( _, point ) ) acumulator ->
                    if rank == 0 then
                        acumulator

                    else
                        point + acumulator
                )
                0
                rankPointedRound

        calculated1stPointRound =
            List.map
                (\( rank, ( index, point ) ) ->
                    if rank == 0 then
                        ( index, negate totalPointsWithout1st )

                    else
                        ( index, point )
                )
                rankPointedRound

        calculatedIntRound =
            calculated1stPointRound
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> Array.fromList
    in
    { chicha = round.chicha
    , points = calculatedIntRound
    }


phrase =
    { pointBalance = "ポイント収支"
    , pointBalanceIncludeChip = "チップ込収支"
    , chip = "チップ(枚数)"
    , balance = "収支"
    , totalBalance = "ゲーム代込み収支"
    , editLogConfigRate = "レート"
    , editLogConfigChipRate = "レート(チップ)"
    , editLogConfigGameFee = "ゲーム代"
    , editLogConfigHavePoint = "持ち点"
    , editLogConfigReturnPoint = "返し"
    , editLogConfigRankPointFirst = "ウマ(2, 3着)"
    , editLogConfigRankPointSecond = "ウマ(1, 4着)"
    , openEditLogConfigArea = "設定を開く"
    , closeEditLogConfigArea = "設定を閉じる"
    , openHowToUseArea = "使い方を開く"
    , closeHowToUseArea = "使い方を閉じる"
    , addRow = "行を追加する"
    , inputPoint = "🖋"
    }



-- Subs


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ fetchedLog FetchedLog, listenedLog ListenedLog ]



-- Ports


port updateLog : LogDto4 -> Cmd msg


port fetchLog : String -> Cmd msg


port fetchedLog : (LogDto4 -> msg) -> Sub msg


port listenLog : String -> Cmd msg


port listenedLog : (LogDto4 -> msg) -> Sub msg
