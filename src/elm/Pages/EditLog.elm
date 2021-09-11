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
import Expands.Array as ExArray
import Expands.Html as ExHtml
import Expands.Maybe as ExMaybe
import Expands.String as ExString
import Expands.Time as ExTime
import Expands.Tuple as ExTuple
import Html exposing (Html, div, img, input, label, p, table, td, text, th, tr)
import Html.Attributes exposing (checked, class, for, id, name, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.EditLog.Chips as Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (LogDto4)
import Pages.EditLog.Log as Log exposing (Log)
import Pages.EditLog.LogConfig exposing (LogConfig, RankPoint)
import Pages.EditLog.Phrase as Phrase
import Pages.EditLog.Players as Players exposing (Players)
import Pages.EditLog.Rounds as Rounds exposing (Kaze, Point, Round, SeatingOrder)
import Pages.EditLog.SeatingOrderInput exposing (SeatingOrderInput)
import Process
import Route exposing (Route)
import Session exposing (Session)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length
import Task exposing (Task)
import Time
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
    , currentTime : Maybe Time.Posix
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


type ModalStatus
    = Hide
      -- Shown {編集中のroundIndex} {同点者がいるかどうか}
    | Shown Int


initModel : LogId -> Session -> Model
initModel logId session =
    { session = session
    , logId = logId
    , pageStatus = Loading
    , currentTime = Nothing
    }


initCmd : LogId -> Cmd Msg
initCmd logId =
    Cmd.batch
        [ fetchLog logId
        , listenLog logId
        , Task.perform SetTime initTimeTask
        ]


initTimeTask : Task Never Time.Posix
initTimeTask =
    Time.now


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


initPageModel : Time.Posix -> PageModel
initPageModel currentTime =
    { log = Log.initLog currentTime
    , uiStatus = initUIStatus
    }


toSession : Model -> Session
toSession { session } =
    session


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



-- msg, update


type Msg
    = SetTime Time.Posix
    | ChangedPlayerName Int String
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
    | ClickedEditRoundButton Int Round
    | ClickedCloseInputPointModalButton
    | ClickedSeatingOrderRadio Int Int Round Kaze


sleep50ms : Task Never ()
sleep50ms =
    Process.sleep 50


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ logId, pageStatus, currentTime } as m) =
    case pageStatus of
        Loading ->
            case msg of
                SetTime now ->
                    -- iphone でのデバッグ用
                    -- ( { m | currentTime = Just now, pageStatus = Loaded { log = dto4ToLog Iphone.log, uiStatus = initUIStatus } }, fetchLog "asd" )
                    ( { m | currentTime = Just now }, Cmd.none )

                FetchedLog dto4 ->
                    case dto4ToLog dto4 of
                        Just log ->
                            ( { m | pageStatus = Loaded { log = log, uiStatus = initUIStatus } }, Cmd.none )

                        Nothing ->
                            ( { m | pageStatus = Loading }, Cmd.none )

                FetchedLogButNoLog () ->
                    case currentTime of
                        Just currentTime_ ->
                            ( { m | pageStatus = Loaded <| initPageModel currentTime_ }, Cmd.none )

                        Nothing ->
                            -- ないとは思うけど現在時刻の取得よりも firebase への fetch が早かったら 50ms 待ってリトライ
                            ( m, Task.perform (\_ -> FetchedLogButNoLog ()) sleep50ms )

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
                            { log | players = StaticArray.set (Index.fromModBy Length.four index) playerName log.players }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                ChangedPoint roundIndex playerIndex point ->
                    let
                        updatedRounds =
                            Rounds.updatePoints
                                { point = point
                                , rounds = rounds
                                , roundIndex = roundIndex
                                , playerIndex = playerIndex
                                }

                        nextLog =
                            { log | rounds = updatedRounds }
                    in
                    ( { m | pageStatus = Loaded { pageModel | log = nextLog } }, updateLog <| toLogDto4 logId nextLog )

                ChangedChip playerIndex chip ->
                    let
                        nextLog =
                            { log | chips = StaticArray.set (Index.fromModBy Length.four playerIndex) chip log.chips }

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
                    case dto4ToLog dto4 of
                        Just log_ ->
                            ( { m | pageStatus = Loaded { log = log_, uiStatus = initUIStatus } }, Cmd.none )

                        Nothing ->
                            ( { m | pageStatus = Loading }, Cmd.none )

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

                ClickedEditRoundButton roundIndex round_ ->
                    let
                        nextSeatingOrderInput =
                            Maybe.withDefault
                                pageModel.uiStatus.seatingOrderInput
                                (Rounds.getSeatingOrderInput round_)
                    in
                    ( { m
                        | pageStatus =
                            Loaded
                                { pageModel
                                    | uiStatus =
                                        { uiStatus
                                            | editRoundModalState = Shown roundIndex
                                            , seatingOrderInput = nextSeatingOrderInput
                                        }
                                }
                      }
                    , Cmd.none
                    )

                ClickedCloseInputPointModalButton ->
                    ( { m | pageStatus = Loaded { pageModel | uiStatus = { uiStatus | editRoundModalState = Hide } } }, Cmd.none )

                ClickedSeatingOrderRadio playerIndex roundIndex round_ kaze ->
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
                                -- 起こり得ないパターン
                                Loading ->
                                    log

                                Loaded pageModel_ ->
                                    let
                                        logUpdated =
                                            pageModel_.log
                                    in
                                    if isDoneSeatingOrderInput pageModel_.uiStatus.seatingOrderInput then
                                        let
                                            nextRounds =
                                                Array.set
                                                    roundIndex
                                                    (Rounds.updateSeatingOrder pageModel_.uiStatus.seatingOrderInput round_)
                                                    rounds
                                        in
                                        { logUpdated | rounds = nextRounds }

                                    else
                                        logUpdated

                        nextPageStatus =
                            case modelSeatingOrderInputUpdated.pageStatus of
                                Loading ->
                                    Loading

                                Loaded pageModel_ ->
                                    Loaded { pageModel_ | log = nextLog }

                        nextModel =
                            { modelSeatingOrderInputUpdated | pageStatus = nextPageStatus }
                    in
                    ( nextModel, updateLog <| toLogDto4 logId nextLog )

                _ ->
                    ( m, Cmd.none )



-- Dto


dto4ToLog : LogDto4 -> Maybe Log
dto4ToLog logDto4 =
    Maybe.map2
        (\players_ chips_ ->
            { createdAt = Time.millisToPosix logDto4.createdAt
            , players = players_
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
            , rounds = Array.map Rounds.roundFromDto logDto4.rounds
            , chips = chips_
            }
        )
        (Players.fromDto logDto4.players)
        (Chips.fromDto logDto4.chips)


toLogDto4 : LogId -> Log -> LogDto4
toLogDto4 logId log =
    { createdAt = Time.posixToMillis log.createdAt
    , logId = logId
    , players = StaticArray.toArray log.players
    , rate = ExString.toIntValue log.logConfig.rate
    , chipRate = ExString.toIntValue log.logConfig.chipRate
    , gameFee = ExString.toIntValue log.logConfig.gameFee
    , rankPoint = Array.fromList [ ExString.toIntValue <| Tuple.first log.logConfig.rankPoint, ExString.toIntValue <| Tuple.second log.logConfig.rankPoint ]
    , havePoint = ExString.toIntValue log.logConfig.havePoint
    , returnPoint = ExString.toIntValue log.logConfig.returnPoint
    , rounds = Array.map Rounds.toRoundObj4 log.rounds
    , chips =
        log.chips
            |> StaticArray.toArray
            |> ExArray.toIntArray
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
                            let
                                round =
                                    Array.get
                                        roundIndex
                                        log.rounds
                                        |> Maybe.withDefault Rounds.initRound4
                            in
                            viewPointInputModal log.players round roundIndex uiStatus.seatingOrderInput
            in
            div [ class "editLog_container" ]
                [ viewHeader
                , viewCreatedAt log.createdAt
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


viewHeader : Html Msg
viewHeader =
    div [ class "editLog_header" ]
        [ viewBackToHome
        ]


viewBackToHome : Html Msg
viewBackToHome =
    div [ class "editLog_logo" ]
        [ UI.viewLink
            { phrase = Phrase.phrase.logo
            , path = Route.routes.home
            , cls = "button_primary"
            }
        ]


viewCreatedAt : Time.Posix -> Html msg
viewCreatedAt currentTime =
    div [ class "editLog_createdAt" ] [ currentTime |> ExTime.posixToYmdhM |> text ]


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
    let
        phrase =
            if isOpened then
                Phrase.phrase.closeHowToUseArea

            else
                Phrase.phrase.openHowToUseArea
    in
    UI.viewButton
        { phrase = phrase
        , onClickMsg = ClickedHowToUseButton
        , size = UI.Default
        , isDisabled = False
        }


viewToggleLogConfigAreaBottun : Bool -> Html Msg
viewToggleLogConfigAreaBottun isOpened =
    let
        phrase =
            if isOpened then
                Phrase.phrase.closeEditLogConfigArea

            else
                Phrase.phrase.openEditLogConfigArea
    in
    UI.viewButton
        { phrase = phrase
        , onClickMsg = ClickedToggleConfigButton
        , size = UI.Default
        , isDisabled = False
        }


{-| 対局情報編集UI
-}
viewEditLogConfig : LogConfig -> Bool -> Html Msg
viewEditLogConfig { rate, chipRate, gameFee, rankPoint, havePoint, returnPoint } isOpened =
    UI.viewIf isOpened <|
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
viewEditLog { players, chips, rounds, logConfig } =
    let
        totalPoint =
            Rounds.totalPoint
                rounds
                logConfig.rankPoint
                logConfig.havePoint
                logConfig.returnPoint

        totalPointIncludeChip =
            Rounds.calculateTotalPointIncludeChip (ExString.toIntValue logConfig.chipRate) totalPoint chips

        totalBalanceExcludeGameFee =
            Rounds.calculateTotalBalanceExcludeGameFee (ExString.toIntValue logConfig.rate) totalPointIncludeChip

        totalBalanceIncludeGameFee =
            Rounds.calculateTotalBalanceIncludeGameFee (ExString.toIntValue logConfig.gameFee) totalBalanceExcludeGameFee
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
            :: List.indexedMap viewInputPlayerCell (StaticArray.toList players)
        )


type alias ViewInputRoundRowConfig =
    { roundIndex : Int
    , round : Round
    , rankPoint : RankPoint
    , havePoint : Point
    , returnPoint : Point
    }


{-| 点数表示行
-}
viewInputRoundRow : ViewInputRoundRowConfig -> Html Msg
viewInputRoundRow { roundIndex, round, rankPoint, havePoint, returnPoint } =
    let
        points =
            if not <| Rounds.isDefaultPoints (Debug.log "round-----" round) then
                Rounds.calculateRoundFromRawPoint
                    { rankPoint = ExTuple.toIntTuple rankPoint
                    , round = Rounds.toIntRound round
                    , havePoint = ExString.toIntValue havePoint
                    , returnPoint = ExString.toIntValue returnPoint
                    }
                    |> Rounds.toStringRound
                    |> Rounds.getPoints

            else
                Rounds.getPoints round

        viewShowPointCell_ =
            if Rounds.isDefaultPoints round then
                List.map
                    viewShowPointCell
                    (round |> Rounds.initPoint |> Array.toList)

            else
                points
                    |> Array.map viewShowPointCell
                    |> Array.toList
    in
    tr [ class "editLog_tr" ]
        (td
            [ class "editLog_calculatedCell" ]
            [ div [ class "editLog_logNumberCellContainer" ]
                [ div [] [ viewInputPointButton roundIndex round ]
                , div [] [ text <| String.fromInt (roundIndex + 1) ]
                ]
            ]
            :: viewShowPointCell_
        )


{-| チップ入力行
-}
viewInputChipsRow : String -> Chips -> Html Msg
viewInputChipsRow title chips =
    tr [ class "editLog_tr" ]
        (td [ class "editLog_title" ]
            (ExHtml.stringToHtmlIncludingBr title)
            :: List.indexedMap
                (\index chip -> viewInputChipsCell index chip)
                (StaticArray.toList chips)
        )


{-| 計算結果行
-}
viewCalculatedRow : String -> Array Int -> Html msg
viewCalculatedRow title calculatedValues =
    tr [ class "editLog_tr" ]
        (td [ class "editLog_title" ] (ExHtml.stringToHtmlIncludingBr title)
            :: (List.map viewCalculatedCell <| Array.toList calculatedValues)
        )


{-| プレイヤー名入力マス
-}
viewInputPlayerCell : Int -> String -> Html Msg
viewInputPlayerCell playerIndex playerName =
    th
        [ class "editLog_th" ]
        [ input
            [ class "editLog_inputCellInput"
            , value playerName
            , onInput <| ChangedPlayerName playerIndex
            ]
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
            , type_ "number"

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
            ]
            []
        ]


{-| 表示用マス
-}
viewShowPointCell : String -> Html Msg
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
            , type_ "number"

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


viewInputPointButton : Int -> Round -> Html Msg
viewInputPointButton index round =
    img [ class "editLog_iconEdit", src "%PUBLIC_URL%/icon-edit.svg", onClick <| ClickedEditRoundButton index round ] []


{-| FIXME: 数字以外を入力すると入力欄が blank になる
-}
viewPointInputModal : Players -> Round -> Int -> SeatingOrderInput -> Html Msg
viewPointInputModal players round roundIndex seatingOrderInput =
    UI.viewModal <|
        div
            [ class "editLog_inputPointModalContentContainer" ]
            [ table [ class "editLog_table" ]
                [ tr [ class "editLog_tr" ]
                    (List.map
                        viewShowPointCell
                        (StaticArray.toList players)
                    )
                , tr [ class "editLog_tr" ]
                    (List.indexedMap
                        (\index_ point -> viewInputPointCell roundIndex index_ point)
                        ((Rounds.unwrapRound >> .points >> Array.toList) round)
                    )
                ]
            , viewInputSeatingOrder roundIndex round seatingOrderInput
                |> UI.viewIf
                    (Rounds.needsSeatingOrderInput round)
            , UI.viewButton
                { phrase = "一覧に戻る"
                , size = UI.Default
                , onClickMsg = ClickedCloseInputPointModalButton
                , isDisabled =
                    Rounds.needsSeatingOrderInput round
                        && isInvalidSeatingOrderInput seatingOrderInput
                }
            ]


viewInputSeatingOrder : Int -> Round -> SeatingOrderInput -> Html Msg
viewInputSeatingOrder roundIndex round seatingOrderInput =
    let
        viewRadioButton : Kaze -> Int -> Point -> Html Msg
        viewRadioButton kaze playerIndex _ =
            div [ class "editLog_chichaRadio" ]
                [ input
                    [ type_ "radio"
                    , id (String.fromInt playerIndex)
                    , name (String.fromInt playerIndex ++ Rounds.kazeToString kaze)
                    , checked <| Rounds.isRadioButtonChecked round kaze playerIndex seatingOrderInput
                    , onClick <| ClickedSeatingOrderRadio playerIndex roundIndex round kaze
                    ]
                    []
                ]

        viewInvalidSeatingOrderMessage =
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
                                ((Rounds.unwrapRound >> .points >> Array.toList) round)
                            )
                        ]
                )
                Rounds.allKazes
            )
        , div [] [ text "同点者がいるため半荘開始時の座順を入力してください" ]
        , viewInvalidSeatingOrderMessage
        ]



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
