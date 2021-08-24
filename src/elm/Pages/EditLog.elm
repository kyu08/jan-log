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
import Dtos.LogDto exposing (LogDto4)
import EditLog.Chips as Chips exposing (Chips)
import EditLog.Log as Log exposing (Log)
import EditLog.LogConfig as LogConfig exposing (LogConfig, RankPoint)
import EditLog.Phrase as Phrase
import EditLog.Players as Players exposing (Players)
import EditLog.Rounds as Rounds exposing (Kaze, Point, Round, Rounds, SeatingOrder)
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
    , pageStatus : PageStatus
    }


type PageStatus
    = Loading
    | Loaded PageModel


type alias PageModel =
    { log : Log
    , uiStatus : UIStatus
    }


type alias UIStatus =
    { isOpenedConfigArea : Bool
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
    , pageStatus = Loading
    }


initUIStatus : UIStatus
initUIStatus =
    { isOpenedConfigArea = False
    , isOpenedHowToUseArea = False
    , editRoundModalState = Hide
    , seatingOrderInput =
        { ton = Nothing
        , nan = Nothing
        , sha = Nothing
        , pei = Nothing
        }
    }


initPageModel : PageModel
initPageModel =
    { log = Log.initLog
    , uiStatus = initUIStatus
    }


initCmd : LogId -> Cmd msg
initCmd logId =
    Cmd.batch
        [ fetchLog logId
        , listenLog logId
        ]


toSession : Model -> Session
toSession { session } =
    session


toSeatingOrderInput : PageStatus -> Maybe SeatingOrderInput
toSeatingOrderInput pageStatus =
    case pageStatus of
        Loading ->
            Nothing

        Loaded pageModel ->
            Just pageModel.uiStatus.seatingOrderInput


isDoneSeatingOrderInput : SeatingOrderInput -> Bool
isDoneSeatingOrderInput { ton, nan, sha, pei } =
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
                        _ :: [] ->
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
    | FetchedLogButNoLog ()
    | ChangedRankPointFirst String
    | ChangedRankPointSecond String
    | ChangedHavePoint String
    | ChangedReturnPoint String
    | ClickedEditRoundButton Int
    | ClickedCloseInputPointModalButton
    | ClickedSeatingOrderRadio Int Int Round Kaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ logId, pageStatus } as m) =
    case pageStatus of
        Loading ->
            case msg of
                FetchedLog dto4 ->
                    ( { m | pageStatus = Loaded { log = dto4ToLog dto4, uiStatus = initUIStatus } }, Cmd.none )

                FetchedLogButNoLog () ->
                    ( { m | pageStatus = Loaded initPageModel }, Cmd.none )

                _ ->
                    ( m, Cmd.none )

        Loaded pageModel ->
            let
                { uiStatus, log } =
                    pageModel

                { seatingOrderInput } =
                    uiStatus

                { logConfig, rounds } =
                    log
            in
            case msg of
                ChangedPlayerName index playerName ->
                    let
                        nextLog =
                            { log | players = Array.set index playerName log.players }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

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
                                log.rounds

                        maybeUpdatedRounds =
                            Maybe.map
                                (updateRound point)
                                (Array.get roundIndex log.rounds)
                    in
                    case maybeUpdatedRounds of
                        Just updatedRound ->
                            let
                                nextLog =
                                    { log | rounds = updatedRound }
                            in
                            ( { m | pageStatus = Loaded { pageModel | log = nextLog } }, updateLog <| toLogDto4 logId nextLog )

                        Nothing ->
                            ( m, Cmd.none )

                ChangedChip playerIndex chip ->
                    let
                        nextLog =
                            { log | chips = Array.set playerIndex chip log.chips }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    -- TODO: ↓これをまとめてやってくれる関数を定義する
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedRate inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rate = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedChipRate inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | chipRate = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedGameFee inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | gameFee = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ClickedAddRowButton ->
                    ( { m | pageStatus = Loaded { pageModel | log = { log | rounds = Array.push Rounds.initRound4 log.rounds } } }, Cmd.none )

                ClickedToggleConfigButton ->
                    ( { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | isOpenedConfigArea = not uiStatus.isOpenedConfigArea } } }
                    , Cmd.none
                    )

                ClickedHowToUseButton ->
                    ( { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | isOpenedHowToUseArea = not uiStatus.isOpenedHowToUseArea } } }
                    , Cmd.none
                    )

                ListenedLog dto4 ->
                    ( { m | pageStatus = Loaded { pageModel | log = dto4ToLog dto4 } }, Cmd.none )

                ChangedRankPointFirst rankpointFirst ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rankPoint = Tuple.mapFirst (\_ -> rankpointFirst) logConfig.rankPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedRankPointSecond rankpointSecond ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rankPoint = Tuple.mapSecond (\_ -> rankpointSecond) logConfig.rankPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedReturnPoint returnPoint ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | returnPoint = returnPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedHavePoint havePoint ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | havePoint = havePoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ClickedEditRoundButton roundIndex ->
                    ( { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | editRoundModalState = Shown roundIndex } } }, Cmd.none )

                ClickedCloseInputPointModalButton ->
                    ( { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | editRoundModalState = Hide } } }, Cmd.none )

                ClickedSeatingOrderRadio playerIndex roundIndex round kaze ->
                    let
                        modelSeatingOrderInputUpdated =
                            case kaze of
                                Rounds.Ton ->
                                    { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | seatingOrderInput = { seatingOrderInput | ton = Just playerIndex } } } }

                                Rounds.Nan ->
                                    { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | seatingOrderInput = { seatingOrderInput | nan = Just playerIndex } } } }

                                Rounds.Sha ->
                                    { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | seatingOrderInput = { seatingOrderInput | sha = Just playerIndex } } } }

                                Rounds.Pei ->
                                    { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | seatingOrderInput = { seatingOrderInput | pei = Just playerIndex } } } }

                        nextLog =
                            case modelSeatingOrderInputUpdated.pageStatus of
                                Loading ->
                                    log

                                Loaded pageModel_ ->
                                    if isDoneSeatingOrderInput pageModel_.uiStatus.seatingOrderInput then
                                        let
                                            nextRounds =
                                                Array.set
                                                    roundIndex
                                                    { round | seatingOrder = toSeatingOrder pageModel.uiStatus.seatingOrderInput }
                                                    rounds

                                            log_ =
                                                pageModel_.log
                                        in
                                        { log_ | rounds = nextRounds }

                                    else
                                        log

                        nextModel =
                            { modelSeatingOrderInputUpdated | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                _ ->
                    ( m, Cmd.none )



-- Dto


dto4ToLog : LogDto4 -> Log
dto4ToLog logDto4 =
    { players = logDto4.players
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
    , rounds = Array.map Rounds.toStringRound4 logDto4.rounds
    , chips = ExArray.toStringArray logDto4.chips
    }


toLogDto4 : LogId -> Log -> LogDto4
toLogDto4 logId log =
    { logId = logId
    , players = log.players
    , rate = ExString.toIntValue log.logConfig.rate
    , chipRate = ExString.toIntValue log.logConfig.chipRate
    , gameFee = ExString.toIntValue log.logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toIntValue <| Tuple.first log.logConfig.rankPoint, ExString.toIntValue <| Tuple.second log.logConfig.rankPoint ]
    , havePoint = ExString.toIntValue log.logConfig.havePoint
    , returnPoint = ExString.toIntValue log.logConfig.returnPoint
    , rounds = Array.map Rounds.toRoundObj4 log.rounds
    , chips = ExArray.toIntArray log.chips
    }



-- VIEW


view : Model -> Html Msg
view { pageStatus } =
    case pageStatus of
        Loading ->
            text "loading"

        Loaded pageModel ->
            let
                { uiStatus, log } =
                    pageModel

                viewPointInputModal_ =
                    case uiStatus.editRoundModalState of
                        Hide ->
                            UI.viewBlank

                        Shown roundIndex ->
                            case Array.get roundIndex log.rounds of
                                Nothing ->
                                    UI.viewBlank

                                Just round ->
                                    viewPointInputModal log.players round roundIndex uiStatus.seatingOrderInput
            in
            div [ class "editLog_container" ]
                [ viewCreatedAt
                , viewEditLog log
                , UI.viewButton { phrase = Phrase.phrase.addRow, onClickMsg = ClickedAddRowButton, size = UI.Default, isDisabled = False }
                , viewToggleLogConfigAreaBottun
                    uiStatus.isOpenedConfigArea
                , viewEditLogConfig
                    log.logConfig
                    uiStatus.isOpenedConfigArea
                , viewToggleHowToUseButton uiStatus.isOpenedHowToUseArea
                , viewHowToUse uiStatus.isOpenedHowToUseArea
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
viewEditLog : Log -> Html Msg
viewEditLog log =
    let
        totalPoint =
            log.rounds
                |> Array.map
                    (\round ->
                        if not <| Rounds.isDefaultPoints round.points then
                            Rounds.calculateRoundFromRawPoint
                                { rankPoint = ExTuple.toIntTuple log.logConfig.rankPoint
                                , round = Rounds.toIntRound round
                                , havePoint = ExString.toIntValue log.logConfig.havePoint
                                , returnPoint = ExString.toIntValue log.logConfig.returnPoint
                                }

                        else
                            Rounds.toIntRound round
                    )
                |> Array.map .points
                |> Rounds.calculateTotalPoint

        totalPointIncludeChip =
            Rounds.calculateTotalPointIncludeChip (ExString.toIntValue log.logConfig.chipRate) totalPoint log.chips

        totalBalanceExcludeGameFee =
            Rounds.calculateTotalBalanceExcludeGameFee (ExString.toIntValue log.logConfig.rate) totalPointIncludeChip

        totalBalanceIncludeGameFee =
            Rounds.calculateTotalBalanceIncludeGameFee (ExString.toIntValue log.logConfig.gameFee) totalBalanceExcludeGameFee
    in
    table
        [ class "editLog_table" ]
        (viewInputPlayersRow log.players
            :: (Array.toList <|
                    Array.indexedMap
                        (\roundIndex round ->
                            viewInputRoundRow
                                { roundIndex = roundIndex
                                , round = round
                                , rankPoint = log.logConfig.rankPoint
                                , havePoint = log.logConfig.havePoint
                                , returnPoint = log.logConfig.returnPoint
                                }
                        )
                        log.rounds
               )
            ++ [ viewInputChipsRow Phrase.phrase.chip log.chips
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
            Rounds.calculateRoundFromRawPoint
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
                        (Rounds.needsSeatingOrderInput round.points)
                , UI.viewButton
                    { phrase = "一覧に戻る"
                    , size = UI.Default
                    , onClickMsg = ClickedCloseInputPointModalButton
                    , isDisabled =
                        Rounds.needsSeatingOrderInput round.points
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
            UI.viewIf (isDoneSeatingOrderInput seatingOrderInput && isInvalidSeatingOrderInput seatingOrderInput) <|
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


viewCreatedAt : Html msg
viewCreatedAt =
    div [ class "editLog_createdAt" ] [ text "2020/20/20" ]



-- Subs


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ fetchedLog FetchedLog
        , listenedLog ListenedLog
        , fetchedLogButNoLog FetchedLogButNoLog
        ]



-- Ports


port updateLog : LogDto4 -> Cmd msg


port fetchLog : String -> Cmd msg


port fetchedLog : (LogDto4 -> msg) -> Sub msg


port fetchedLogButNoLog : (() -> msg) -> Sub msg


port listenLog : String -> Cmd msg


port listenedLog : (LogDto4 -> msg) -> Sub msg
