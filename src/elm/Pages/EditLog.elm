port module Pages.EditLog exposing
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
import Common.LogId exposing (LogId)
import Dtos.LogDto exposing (LogDto4, RoundObj4Dto)
import EditLog.Chips as Chips exposing (Chips)
import EditLog.LogConfig as LogConfig exposing (LogConfig, RankPoint)
import EditLog.Phrase as Phrase
import EditLog.Players as Players exposing (Players)
import EditLog.Rounds as Rounds exposing (IntRound, Kaze, Point, Round, Rounds, SeatingOrder)
import Expands.Array as ExArray
import Expands.Maybe as ExMaybe
import Expands.String as ExString
import Expands.Tuple as ExTuple
import Html exposing (Html, div, input, label, p, table, td, text, th, tr)
import Html.Attributes exposing (checked, class, for, id, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session)
import UI



-- types


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
    , players : Players
    , logConfig : LogConfig
    , rounds : Rounds
    , chips : Chips

    -- state for UI
    , isOpenedConfigArea : Bool
    , isOpenedHowToUseArea : Bool
    , editRoundModalState : ModalStatus
    , seatingOrderInput : SeatingOrderInput
    }


type alias SeatingOrderInput =
    { ton : Maybe Int
    , nan : Maybe Int
    , sha : Maybe Int
    , pei : Maybe Int
    }


type ModalStatus
    = Hide
      -- Shown {編集中のroundIndex} {同点者がいるかどうか}
    | Shown Int


initModel : LogId -> Session -> Model
initModel logId session =
    { session = session
    , logId = logId
    , players = Players.initPlayers
    , logConfig = LogConfig.initLogConfig
    , rounds = Rounds.initRounds
    , chips = Chips.initChips
    , isOpenedConfigArea = False
    , isOpenedHowToUseArea = False
    , editRoundModalState = Hide
    , seatingOrderInput =
        { ton = Nothing
        , nan = Nothing
        , sha = Nothing
        , pei = Nothing
        }
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


isAllSeatingOrderInput : SeatingOrderInput -> Bool
isAllSeatingOrderInput { ton, nan, sha, pei } =
    ExMaybe.isJust ton
        && ExMaybe.isJust nan
        && ExMaybe.isJust sha
        && ExMaybe.isJust pei


isInvalidSeatingOrderInput : SeatingOrderInput -> Bool
isInvalidSeatingOrderInput { ton, nan, sha, pei } =
    Maybe.map4
        (\ton_ nan_ sha_ pei_ ->
            let
                -- 各家に別の playerIndex が入力されていない場合に True をかえす
                recursive kazes =
                    case kazes of
                        head :: [] ->
                            False

                        head :: tail ->
                            if List.any ((==) head) tail then
                                True

                            else
                                recursive tail

                        [] ->
                            False
            in
            recursive [ ton_, nan_, sha_, pei_ ]
        )
        ton
        nan
        sha
        pei
        |> Maybe.withDefault True


toSeatingOrder : SeatingOrderInput -> Maybe SeatingOrder
toSeatingOrder { ton, nan, sha, pei } =
    Maybe.map4
        (\ton_ nan_ sha_ pei_ -> { ton = ton_, nan = nan_, sha = sha_, pei = pei_ })
        ton
        nan
        sha
        pei



-- msg, update


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
    | NoOp
    | ClickedCloseInputPointModalButton
    | ClickedSeatingOrderRadio Int Int Round Kaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rounds, players, logConfig, chips, isOpenedConfigArea, isOpenedHowToUseArea, seatingOrderInput } as m) =
    case msg of
        ChangedPlayerName index playerName ->
            let
                nextModel =
                    { m | players = Array.set index playerName players }
            in
            ( nextModel, updateLog <| toLogDto4 nextModel )

        ChangedPoint roundIndex playerIndex point ->
            let
                updateRound point_ round_ =
                    Array.set roundIndex
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
                        (Array.get roundIndex rounds)

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
            ( { m | rounds = Array.push Rounds.initRound4 rounds }, Cmd.none )

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

        ClickedEditRoundButton roundIndex ->
            ( { m | editRoundModalState = Shown roundIndex }, Cmd.none )

        NoOp ->
            ( m, Cmd.none )

        ClickedCloseInputPointModalButton ->
            ( { m | editRoundModalState = Hide }, Cmd.none )

        ClickedSeatingOrderRadio playerIndex roundIndex round kaze ->
            let
                nextModel =
                    case kaze of
                        Rounds.Ton ->
                            { m | seatingOrderInput = { seatingOrderInput | ton = Just playerIndex } }

                        Rounds.Nan ->
                            { m | seatingOrderInput = { seatingOrderInput | nan = Just playerIndex } }

                        Rounds.Sha ->
                            { m | seatingOrderInput = { seatingOrderInput | sha = Just playerIndex } }

                        Rounds.Pei ->
                            { m | seatingOrderInput = { seatingOrderInput | pei = Just playerIndex } }

                -- TODO: 各家がひとりずつ選択されてなかったら警告をだす
                nextRounds =
                    if isAllSeatingOrderInput nextModel.seatingOrderInput then
                        Array.set roundIndex { round | seatingOrder = toSeatingOrder nextModel.seatingOrderInput } rounds

                    else
                        rounds
            in
            ( { nextModel | rounds = nextRounds }, updateLog <| toLogDto4 nextModel )



-- Dto


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
                    (String.fromInt <| ExArray.getArrayElement 0 logDto4.rankPoint)
                    (String.fromInt <| ExArray.getArrayElement 1 logDto4.rankPoint)
            , havePoint = String.fromInt logDto4.havePoint
            , returnPoint = String.fromInt logDto4.returnPoint
            }
        , players = logDto4.players
        , rounds = Array.map Rounds.toStringRound4 logDto4.rounds
        , chips = ExArray.toStringArray logDto4.chips
    }


toLogDto4 : Model -> LogDto4
toLogDto4 { logId, logConfig, players, rounds, chips } =
    { logId = logId
    , players = players
    , rate = ExString.toIntValue logConfig.rate
    , chipRate = ExString.toIntValue logConfig.chipRate
    , gameFee = ExString.toIntValue logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toIntValue <| Tuple.first logConfig.rankPoint, ExString.toIntValue <| Tuple.second logConfig.rankPoint ]
    , havePoint = ExString.toIntValue logConfig.havePoint
    , returnPoint = ExString.toIntValue logConfig.returnPoint
    , rounds = Array.map Rounds.toRoundObj4 rounds
    , chips = ExArray.toIntArray chips
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewPointInputModal_ =
            case model.editRoundModalState of
                Hide ->
                    UI.viewBlank

                Shown roundIndex ->
                    case Array.get roundIndex model.rounds of
                        Nothing ->
                            UI.viewBlank

                        Just round ->
                            viewPointInputModal model.players round roundIndex model.seatingOrderInput
    in
    div [ class "editLog_container" ]
        [ viewEditLog model
        , UI.viewButton { phrase = Phrase.phrase.addRow, onClickMsg = ClickedAddRowButton, size = UI.Default, isDisabled = False }
        , viewToggleLogConfigAreaBottun
            model.isOpenedConfigArea
        , viewEditLogConfig
            model.logConfig
            model.isOpenedConfigArea
        , viewToggleHowToUseButton model.isOpenedHowToUseArea
        , viewHowToUse model.isOpenedHowToUseArea
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
            { phrase = Phrase.phrase.closeHowToUseArea
            , onClickMsg = ClickedHowToUseButton
            , size = UI.Default
            , isDisabled = False
            }

    else
        UI.viewButton
            { phrase = Phrase.phrase.openHowToUseArea
            , onClickMsg = ClickedHowToUseButton
            , size = UI.Default
            , isDisabled = False
            }


viewToggleLogConfigAreaBottun : Bool -> Html Msg
viewToggleLogConfigAreaBottun isOpened =
    if isOpened then
        UI.viewButton
            { phrase = Phrase.phrase.closeEditLogConfigArea
            , onClickMsg = ClickedToggleConfigButton
            , size = UI.Default
            , isDisabled = False
            }

    else
        UI.viewButton
            { phrase = Phrase.phrase.openEditLogConfigArea
            , onClickMsg = ClickedToggleConfigButton
            , size = UI.Default
            , isDisabled = False
            }


{-| 対局情報編集UI
-}
viewEditLogConfig : LogConfig -> Bool -> Html Msg
viewEditLogConfig { rate, chipRate, gameFee, rankPoint, havePoint, returnPoint } isOpened =
    if isOpened then
        div
            [ class "editLog_logConfigContainer" ]
            [ viewEditLogConfigForm Phrase.phrase.editLogConfigRate rate ChangedRate
            , viewEditLogConfigForm Phrase.phrase.editLogConfigChipRate chipRate ChangedChipRate
            , viewEditLogConfigForm Phrase.phrase.editLogConfigGameFee gameFee ChangedGameFee
            , viewEditLogConfigForm Phrase.phrase.editLogConfigHavePoint havePoint ChangedHavePoint
            , viewEditLogConfigForm Phrase.phrase.editLogConfigReturnPoint returnPoint ChangedReturnPoint
            , viewEditLogConfigForm Phrase.phrase.editLogConfigRankPointFirst (Tuple.first rankPoint) ChangedRankPointFirst
            , viewEditLogConfigForm Phrase.phrase.editLogConfigRankPointSecond (Tuple.second rankPoint) ChangedRankPointSecond
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
viewEditLog { logConfig, players, rounds, chips } =
    let
        totalPoint =
            rounds
                |> Array.map
                    (\round ->
                        if not <| Rounds.isDefaultPoints round.points then
                            calculateRoundFromRawPoint
                                { rankPoint = ExTuple.toIntTuple logConfig.rankPoint
                                , round = Rounds.toIntRound round
                                , havePoint = ExString.toIntValue logConfig.havePoint
                                , returnPoint = ExString.toIntValue logConfig.returnPoint
                                }

                        else
                            Rounds.toIntRound round
                    )
                |> Array.map .points
                |> calculateTotalPoint

        totalPointIncludeChip =
            calculateTotalPointIncludeChip (ExString.toIntValue logConfig.chipRate) totalPoint chips

        totalBalanceExcludeGameFee =
            calculateTotalBalanceExcludeGameFee (ExString.toIntValue logConfig.rate) totalPointIncludeChip

        totalBalanceIncludeGameFee =
            calculateTotalBalanceIncludeGameFee (ExString.toIntValue logConfig.gameFee) totalBalanceExcludeGameFee
    in
    table
        [ class "editLog_table" ]
        (viewInputPlayersRow players
            :: (Array.toList <|
                    Array.indexedMap
                        (\roundIndex round ->
                            viewInputRoundRow
                                { roundIndex = roundIndex
                                , round = round
                                , rankPoint = logConfig.rankPoint
                                , havePoint = logConfig.havePoint
                                , returnPoint = logConfig.returnPoint
                                }
                        )
                        rounds
               )
            ++ [ viewInputChipsRow Phrase.phrase.chip chips
               , viewCalculatedRow Phrase.phrase.pointBalance totalPoint
               , viewCalculatedRow Phrase.phrase.pointBalanceIncludeChip totalPointIncludeChip
               , viewCalculatedRow Phrase.phrase.balance totalBalanceExcludeGameFee
               , viewCalculatedRow Phrase.phrase.totalBalance totalBalanceIncludeGameFee
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
    { roundIndex : Int
    , round : Round
    , rankPoint : RankPoint
    , havePoint : Point
    , returnPoint : Point
    }


{-| 点棒入力行
-}
viewInputRoundRow : ViewInputRoundRowConfig -> Html Msg
viewInputRoundRow { roundIndex, round, rankPoint, havePoint, returnPoint } =
    let
        points =
            calculateRoundFromRawPoint
                { rankPoint = ExTuple.toIntTuple rankPoint
                , round = Rounds.toIntRound round
                , havePoint = ExString.toIntValue havePoint
                , returnPoint = ExString.toIntValue returnPoint
                }
                |> Rounds.toStringRound
                >> .points

        viewShowPointCell_ =
            if Rounds.isDefaultPoints round.points then
                Array.map
                    viewShowPointCell
                    round.points
                    |> Array.toList

            else
                points
                    |> Array.map viewShowPointCell
                    |> Array.toList
    in
    tr [ class "editLog_tr" ]
        (td
            [ class "editLog_logNumberCell" ]
            [ viewInputPointButton roundIndex, text <| String.fromInt (roundIndex + 1) ]
            :: viewShowPointCell_
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
viewCalculatedRow title calculatedValues =
    tr [ class "editLog_tr" ]
        (td [ class "editLog_title" ] [ text title ]
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
viewInputPointCell roundIndex playerIndex point =
    td
        [ class "editLog_td" ]
        [ input
            [ class "editLog_inputCellInput"
            , value point
            , onInput <| ChangedPoint roundIndex playerIndex

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
            ]
            []
        ]


{-| 点数表示マス
ここでウマオカとかを計算して表示する
-}
viewShowPointCell : String -> Html Msg
viewShowPointCell point =
    -- viewShowPointCell : Point -> Html Msg
    -- viewShowPointCell point =
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
        { phrase = Phrase.phrase.inputPoint
        , onClickMsg = ClickedEditRoundButton index
        , size = UI.Mini
        , isDisabled = False
        }


{-| FIXME: 数字以外を入力すると入力欄が blank になる
-}
viewPointInputModal : Players -> Round -> Int -> SeatingOrderInput -> Html Msg
viewPointInputModal players round roundIndex seatingOrderInput =
    let
        viewContent =
            div
                [ class "editLog_inputPointModalContentContainer" ]
                [ table [ class "editLog_table" ]
                    [ tr [ class "editLog_tr" ]
                        (List.map
                            viewShowPointCell
                            (Array.toList players)
                        )
                    , tr [ class "editLog_tr" ]
                        (List.indexedMap
                            (\index_ point -> viewInputPointCell roundIndex index_ point)
                            (Array.toList round.points)
                        )
                    ]
                , viewInputSeatingOrder roundIndex round seatingOrderInput
                    |> UI.viewIf
                        (needsSeatingOrderInput round.points)
                , UI.viewButton
                    { phrase = "一覧に戻る"
                    , size = UI.Default
                    , onClickMsg = ClickedCloseInputPointModalButton
                    , isDisabled =
                        needsSeatingOrderInput round.points
                            && isInvalidSeatingOrderInput seatingOrderInput
                    }
                ]
    in
    UI.viewModal viewContent


viewInputSeatingOrder : Int -> Round -> SeatingOrderInput -> Html Msg
viewInputSeatingOrder roundIndex round seatingOrderInput =
    let
        viewRadioButton : Kaze -> Int -> Point -> Html Msg
        viewRadioButton kaze playerIndex _ =
            let
                checked_ =
                    case round.seatingOrder of
                        Just seatingOrder ->
                            Rounds.kazeToSelecter kaze seatingOrder == playerIndex

                        Nothing ->
                            case Rounds.kazeToSelecter kaze seatingOrderInput of
                                Just playerIndex_ ->
                                    playerIndex == playerIndex_

                                Nothing ->
                                    False
            in
            div [ class "editLog_chichaRadio" ]
                [ input
                    [ type_ "radio"
                    , id (String.fromInt playerIndex)
                    , name (String.fromInt playerIndex ++ Rounds.kazeToString kaze)
                    , checked checked_
                    , onClick <| ClickedSeatingOrderRadio playerIndex roundIndex round kaze
                    ]
                    []
                ]

        invalidSeatingOrderMessage =
            UI.viewIf (isAllSeatingOrderInput seatingOrderInput && isInvalidSeatingOrderInput seatingOrderInput) <|
                text "1人ずつ選択してください"
    in
    div []
        [ div []
            (List.map
                (\kaze_ ->
                    div []
                        [ text <|
                            Rounds.kazeToString kaze_
                        , div [ class "editLog_chichaRadioContainer" ]
                            (List.indexedMap
                                (viewRadioButton kaze_)
                                (Array.toList round.points)
                            )
                        ]
                )
                Rounds.allKazes
            )
        , div [] [ text "同点者がいるため半荘開始時の座順を入力してください" ]
        , invalidSeatingOrderMessage
        ]



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
        (Array.initialize 1 (\_ -> ExArray.toIntArray chips))


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
TODO: トビは現状の実装だと場外で(チップなどで)やりとりするしかないので↑の計算方法をやめる。
-}
calculateRoundFromRawPoint : CalculateRoundFromRawPointConfig -> IntRound
calculateRoundFromRawPoint { round, rankPoint, havePoint, returnPoint } =
    let
        -- 順位点が入ってる List
        rankPointArray =
            [ Tuple.second rankPoint
            , Tuple.first rankPoint
            , negate <| Tuple.first rankPoint
            , negate <| Tuple.second rankPoint
            ]

        -- n万点返しした
        returnedRound =
            Array.indexedMap
                (\index point -> ( index, point - returnPoint ))
                round.points

        -- 座順データが存在すれば起家ソートを行う
        chichaSortedRound =
            case round.seatingOrder of
                Just seatingOrder ->
                    Maybe.map4
                        (\ton_ nan_ sha_ pei_ ->
                            [ ton_, nan_, sha_, pei_ ]
                                |> List.reverse
                                |> Array.fromList
                        )
                        (Array.get seatingOrder.ton returnedRound)
                        (Array.get seatingOrder.nan returnedRound)
                        (Array.get seatingOrder.sha returnedRound)
                        (Array.get seatingOrder.pei returnedRound)
                        |> Maybe.withDefault returnedRound

                Nothing ->
                    returnedRound

        -- point でソート
        sortedRound =
            List.reverse <|
                List.sortBy
                    Tuple.second
                    (Array.toList chichaSortedRound)

        -- 順位点を加算
        rankPointedRound =
            List.map2
                (\rankPoint_ ( rank, ( index, point ) ) ->
                    ( rank, ( index, point + rankPoint_ ) )
                )
                rankPointArray
                (List.indexedMap (\rank roundWithIndex -> ( rank, roundWithIndex )) sortedRound)

        -- 2着 ~ 3着 のプレイヤーの合計(1着のポイント計算用に使う)
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

        -- 2 ~ 3 着のポイント合計をマイナスしたものを1着のポイントとして計算する
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
    { seatingOrder = round.seatingOrder
    , points = calculatedIntRound
    }


{-| TODO: 適切なモジュールに移動する
-}
needsSeatingOrderInput : Array Point -> Bool
needsSeatingOrderInput points =
    let
        isDoneInput points_ =
            points_
                |> Array.filter ((/=) "")
                |> Array.length
                |> (<=) 4
    in
    -- 入力が完了していない場合は起家の入力を求めない
    if not <| isDoneInput points then
        False

    else
        -- 入力が完了している場合は同点判定をする
        hasSamePoint points


hasSamePoint : Array Point -> Bool
hasSamePoint points =
    case Array.toList points of
        _ :: [] ->
            False

        [] ->
            False

        head :: tail ->
            if List.any (\point -> point == head) tail then
                True

            else
                hasSamePoint <| Array.fromList tail



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
