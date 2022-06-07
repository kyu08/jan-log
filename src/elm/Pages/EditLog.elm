port module Pages.EditLog exposing
    ( Model
    , Msg
    , initCmd4
    , initCmd5
    , initModel
    , subscriptions
    , toSession
    , update
    , view
    )

import Array exposing (Array)
import Common.LogId exposing (LogId)
import Expands.Html as ExHtml
import Expands.Maybe as ExMaybe
import Expands.String as ExString
import Expands.Time as ExTime
import Expands.Tuple as ExTuple
import Html exposing (Html, div, img, input, label, option, p, select, table, td, text, th, tr)
import Html.Attributes exposing (checked, class, for, id, name, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Http.Miyabq as HttpMiyabq
import Pages.EditLog.Chips as Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (LogDto4, LogDto5)
import Pages.EditLog.Dtos.UserDto exposing (UserDto)
import Pages.EditLog.Log as Log exposing (Log)
import Pages.EditLog.LogConfig exposing (LogConfig, RankPoint)
import Pages.EditLog.Miyabq exposing (isCorrectPassword)
import Pages.EditLog.Phrase as Phrase exposing (phrase)
import Pages.EditLog.Players as Players exposing (Players)
import Pages.EditLog.Rounds as Rounds exposing (Kaze, Point, Round)
import Pages.EditLog.SeatingOrderInput exposing (SeatingOrderInput)
import Process
import Route
import Session exposing (Session)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length
import Task exposing (Task)
import Time
import UI



-- types


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

    -- グループ機能を追加して通算成績を見れるようにする構想があるため、捨てやすいように独立したプロパティにしている
    , miyabq : MiyaBq
    }


type alias UIStatus =
    { isOpenedConfigArea : Bool
    , isOpenedHowToUseArea : Bool
    , editRoundModalState : ModalStatus
    , seatingOrderInput : SeatingOrderInput
    }


type alias MiyaBq =
    { users : List UserDto
    , relation : StaticArray Index.Four Int
    , enteredPassword : String
    , requestStatus : RequestStatus
    }


type RequestStatus
    = None
    | Sending
    | Success
    | Failed


type ModalStatus
    = Hide
      -- Shown {編集中のroundIndex}
    | Shown Int


initModel : LogId -> Session -> Model
initModel logId session =
    { session = session
    , logId = logId
    , pageStatus = Loading
    , currentTime = Nothing
    }


initCmd4 : LogId -> Cmd Msg
initCmd4 logId =
    Cmd.batch
        [ fetchLog4 logId
        , listenLog4 logId
        , Task.perform SetTime initTimeTask
        ]


initCmd5 : LogId -> Cmd Msg
initCmd5 logId =
    Cmd.batch
        [ fetchLog5 logId
        , listenLog5 logId
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


initPageModel4 : Time.Posix -> PageModel
initPageModel4 currentTime =
    { log = Log.initLog4 currentTime
    , uiStatus = initUIStatus
    , miyabq =
        { users = []
        , relation = StaticArray.initialize Length.four (\_ -> 1)
        , enteredPassword = ""
        , requestStatus = None
        }
    }


initPageModel5 : Time.Posix -> PageModel
initPageModel5 currentTime =
    { log = Log.initLog5 currentTime
    , uiStatus = initUIStatus
    , miyabq =
        { users = []
        , relation = StaticArray.initialize Length.four (\_ -> 1)
        , enteredPassword = ""
        , requestStatus = None
        }
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
    | ChangedTobisho Int Int String
    | ChangedChip Int String
    | ChangedRate String
    | ChangedChipRate String
    | ChangedGameFee String
    | ClickedAddRowButton
    | ClickedToggleConfigButton
    | ClickedHowToUseButton
    | FetchedLog4 LogDto4
    | FetchedLog5 LogDto5
    | ListenedLog4 LogDto4
    | ListenedLog5 LogDto5
    | FetchedLogButNoLog4 ()
    | FetchedLogButNoLog5 ()
    | ChangedRankPointFirst String
    | ChangedRankPointSecond String
    | ChangedHavePoint String
    | ChangedReturnPoint String
    | ClickedEditRoundButton Int Round
    | ClickedCloseInputPointModalButton
    | ClickedSeatingOrderRadio Int Int Round Kaze
    | ClickedExportToMiyabqButton
    | MiyabqPostResponse (Result Http.Error String)
    | GotUsersFromMiyabq (Result Http.Error (List UserDto))
    | ChangedMiyabqUser Int String
    | ChangedPW String


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

                FetchedLog4 log4Dto ->
                    case Log.dto4ToLog log4Dto of
                        Just log ->
                            ( { m
                                | pageStatus =
                                    Loaded
                                        { log = log
                                        , uiStatus = initUIStatus
                                        , miyabq =
                                            { users = []
                                            , relation = StaticArray.initialize Length.four (\_ -> 1)
                                            , enteredPassword = ""
                                            , requestStatus = None
                                            }
                                        }
                              }
                            , HttpMiyabq.getUsers GotUsersFromMiyabq
                            )

                        Nothing ->
                            -- データが壊れている場合
                            ( { m | pageStatus = Loading }
                            , HttpMiyabq.getUsers GotUsersFromMiyabq
                            )

                FetchedLog5 log5Dto ->
                    case Log.dto5ToLog log5Dto of
                        Just log ->
                            ( { m
                                | pageStatus =
                                    Loaded
                                        { log = log
                                        , uiStatus = initUIStatus
                                        , miyabq =
                                            { users = []
                                            , relation = StaticArray.initialize Length.four (\_ -> 1)
                                            , enteredPassword = ""
                                            , requestStatus = None
                                            }
                                        }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { m | pageStatus = Loading }, Cmd.none )

                FetchedLogButNoLog4 () ->
                    case currentTime of
                        Just currentTime_ ->
                            ( { m | pageStatus = Loaded <| initPageModel4 currentTime_ }
                            , HttpMiyabq.getUsers GotUsersFromMiyabq
                            )

                        Nothing ->
                            -- ないとは思うけど現在時刻の取得よりも firebase への fetch が早かったら 50ms 待ってリトライ
                            ( m
                            , Cmd.batch
                                [ Task.perform (\_ -> FetchedLogButNoLog4 ()) sleep50ms
                                , HttpMiyabq.getUsers GotUsersFromMiyabq
                                ]
                            )

                FetchedLogButNoLog5 () ->
                    case currentTime of
                        Just currentTime_ ->
                            ( { m | pageStatus = Loaded <| initPageModel5 currentTime_ }, Cmd.none )

                        Nothing ->
                            -- ないとは思うけど現在時刻の取得よりも firebase への fetch が早かったら 50ms 待ってリトライ
                            ( m, Task.perform (\_ -> FetchedLogButNoLog5 ()) sleep50ms )

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
                            { log | players = Players.updatePlayerName index playerName log.players }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

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
                    ( { m | pageStatus = Loaded { pageModel | log = nextLog } }, updateLog logId nextLog )

                ChangedTobisho roundIndex playerIndex tobisho ->
                    let
                        updatedRounds =
                            Rounds.updateTobisho
                                { tobisho = tobisho
                                , rounds = rounds
                                , roundIndex = roundIndex
                                , playerIndex = playerIndex
                                }

                        nextLog =
                            { log | rounds = updatedRounds }
                    in
                    ( { m | pageStatus = Loaded { pageModel | log = nextLog } }, updateLog logId nextLog )

                ChangedChip playerIndex chip ->
                    let
                        nextLog =
                            { log | chips = Chips.update playerIndex chip log.chips }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedRate inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rate = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedChipRate inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | chipRate = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedGameFee inputValue ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | gameFee = inputValue } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

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

                ListenedLog4 dto4 ->
                    case Log.dto4ToLog dto4 of
                        Just log_ ->
                            ( { m
                                | pageStatus =
                                    Loaded
                                        { log = log_
                                        , uiStatus = initUIStatus
                                        , miyabq =
                                            { users = []
                                            , relation = StaticArray.initialize Length.four (\_ -> 1)
                                            , enteredPassword = ""
                                            , requestStatus = None
                                            }
                                        }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { m | pageStatus = Loading }, Cmd.none )

                ListenedLog5 dto5 ->
                    case Log.dto5ToLog dto5 of
                        Just log_ ->
                            ( { m
                                | pageStatus =
                                    Loaded
                                        { log = log_
                                        , uiStatus = initUIStatus
                                        , miyabq =
                                            { users = []
                                            , relation = StaticArray.initialize Length.four (\_ -> 1)
                                            , enteredPassword = ""
                                            , requestStatus = None
                                            }
                                        }
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { m | pageStatus = Loading }, Cmd.none )

                ChangedRankPointFirst rankpointFirst ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rankPoint = Tuple.mapFirst (\_ -> rankpointFirst) logConfig.rankPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedRankPointSecond rankpointSecond ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | rankPoint = Tuple.mapSecond (\_ -> rankpointSecond) logConfig.rankPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedReturnPoint returnPoint ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | returnPoint = returnPoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

                ChangedHavePoint havePoint ->
                    let
                        nextLog =
                            { log | logConfig = { logConfig | havePoint = havePoint } }

                        nextModel =
                            { m | pageStatus = Loaded { pageModel | log = nextLog } }
                    in
                    ( nextModel, updateLog logId nextLog )

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
                    ( nextModel, updateLog logId nextLog )

                ClickedExportToMiyabqButton ->
                    let
                        currentMiyabq =
                            pageModel.miyabq
                    in
                    ( { m | pageStatus = Loaded { pageModel | miyabq = { currentMiyabq | requestStatus = Sending } } }
                    , HttpMiyabq.postResult
                        { resultsDto =
                            Log.toResultDto4
                                { createdAt = pageModel.log.createdAt
                                , playerIds = pageModel.miyabq.relation
                                , rounds = pageModel.log.rounds
                                , chips =
                                    pageModel.log.chips
                                        |> Chips.toArray
                                        |> Array.map ExString.toInt
                                , rankPoint = Tuple.mapBoth ExString.toInt ExString.toInt pageModel.log.logConfig.rankPoint
                                , returnPoint = ExString.toInt pageModel.log.logConfig.returnPoint
                                }
                        , onResponseMsg = MiyabqPostResponse
                        }
                    )

                MiyabqPostResponse response ->
                    let
                        currentMiyabq =
                            pageModel.miyabq
                    in
                    case response of
                        Ok _ ->
                            ( { m | pageStatus = Loaded { pageModel | miyabq = { currentMiyabq | requestStatus = Success } } }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { m | pageStatus = Loaded { pageModel | miyabq = { currentMiyabq | requestStatus = Failed } } }
                            , Cmd.none
                            )

                GotUsersFromMiyabq users ->
                    let
                        currentMiyabq =
                            pageModel.miyabq
                    in
                    case users of
                        Ok users_ ->
                            ( { m | pageStatus = Loaded { pageModel | miyabq = { currentMiyabq | users = users_ } } }
                            , Cmd.none
                            )

                        Err _ ->
                            ( m
                            , Cmd.none
                            )

                ChangedMiyabqUser index userId ->
                    let
                        currentMiyabq =
                            pageModel.miyabq
                    in
                    ( { m
                        | pageStatus =
                            Loaded
                                { pageModel
                                    | miyabq =
                                        { currentMiyabq
                                            | relation =
                                                StaticArray.set
                                                    (Index.fromModBy Length.four index)
                                                    (ExString.toInt userId)
                                                    currentMiyabq.relation
                                        }
                                }
                      }
                    , Cmd.none
                    )

                ChangedPW input ->
                    let
                        currentMiyabq =
                            pageModel.miyabq
                    in
                    ( { m
                        | pageStatus =
                            Loaded
                                { pageModel
                                    | miyabq =
                                        { currentMiyabq
                                            | enteredPassword = input
                                        }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( m, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { pageStatus } =
    case pageStatus of
        Loading ->
            text "loading"

        Loaded pageModel ->
            let
                { uiStatus, log, miyabq } =
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
                , viewEnterPW miyabq.enteredPassword
                , viewMiyabq
                    { miyaBq = miyabq
                    , players = pageModel.log.players
                    }
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
    div [ class "editLog_createdAt" ] [ currentTime |> ExTime.posixToYmd |> text ]


viewHowToUse : Bool -> Html msg
viewHowToUse isOpened =
    if isOpened then
        div [ class "editLog_howToUseContainer" ]
            [ p [] [ text phrase.howToUse1 ]
            , p [] [ text phrase.howToUse2 ]
            , p [] [ text phrase.howToUse3 ]
            , p [] [ text phrase.howToUse4 ]
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


viewEnterPW : String -> Html Msg
viewEnterPW inputPW =
    div []
        [ input [ value inputPW, onInput ChangedPW ] []
        ]


type alias ViewMiyabqConfig =
    { miyaBq : MiyaBq
    , players : Players
    }


viewMiyabq : ViewMiyabqConfig -> Html Msg
viewMiyabq viewMiyabqConfig =
    if isCorrectPassword viewMiyabqConfig.miyaBq.enteredPassword then
        div [ class "editLog_miyabqContainer" ]
            [ viewMiyabqUserSelector viewMiyabqConfig.miyaBq viewMiyabqConfig.players
            , viewMiyabqMessage viewMiyabqConfig.miyaBq.requestStatus
            , viewToggleExportToMiyabqButton viewMiyabqConfig.miyaBq.requestStatus
            ]

    else
        UI.viewBlank


viewMiyabqMessage : RequestStatus -> Html Msg
viewMiyabqMessage requestStatus =
    let
        ( phrase_, cls ) =
            case requestStatus of
                None ->
                    ( "", "" )

                Sending ->
                    ( phrase.exportToMiyabqSending, "" )

                Success ->
                    ( phrase.exportToMiyabqSuccess, "editLog_miyabqMessageSuccess" )

                Failed ->
                    ( phrase.exportToMiyabqFailed, "editLog_miyabqMessageFailed" )
    in
    div
        [ class "editLog_miyabqMessage"
        , class cls
        ]
        [ text phrase_ ]


viewToggleExportToMiyabqButton : RequestStatus -> Html Msg
viewToggleExportToMiyabqButton requestStatus =
    UI.viewIf (not <| requestStatus == Success) <|
        UI.viewButton
            { phrase = phrase.exportToMiyabq
            , onClickMsg = ClickedExportToMiyabqButton
            , size = UI.Default
            , isDisabled = False
            }


viewMiyabqUserSelector : MiyaBq -> Players -> Html Msg
viewMiyabqUserSelector miyaBqData players =
    div
        []
        (players
            |> Players.toList
            |> List.indexedMap
                (\index p ->
                    div []
                        [ text (p ++ ": ")
                        , select [ onInput <| ChangedMiyabqUser index ]
                            (List.map
                                (\userDto ->
                                    option
                                        [ value <| String.fromInt userDto.id
                                        , selected <|
                                            userDto.id
                                                == StaticArray.get (Index.fromModBy Length.four index) miyaBqData.relation
                                        ]
                                        [ text userDto.name ]
                                )
                                miyaBqData.users
                            )
                        ]
                )
        )


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
                logConfig.returnPoint

        totalPointIncludeChip =
            Rounds.calculateTotalPointIncludeChip (ExString.toInt logConfig.chipRate) totalPoint chips

        totalBalanceExcludeGameFee =
            Rounds.calculateTotalBalanceExcludeGameFee (ExString.toInt logConfig.rate) totalPointIncludeChip

        totalBalanceIncludeGameFee =
            Rounds.calculateTotalBalanceIncludeGameFee (ExString.toInt logConfig.gameFee) totalBalanceExcludeGameFee
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
            :: List.indexedMap viewInputPlayerCell (players |> Players.toArray |> Array.toList)
        )


type alias ViewInputRoundRowConfig =
    { roundIndex : Int
    , round : Round
    , rankPoint : RankPoint
    , returnPoint : Point
    }


{-| 点数表示行
-}
viewInputRoundRow : ViewInputRoundRowConfig -> Html Msg
viewInputRoundRow { roundIndex, round, rankPoint, returnPoint } =
    let
        points =
            if not <| Rounds.isDefaultRound round then
                Rounds.calculateRoundFromRawPoint
                    { rankPoint = ExTuple.toIntTuple rankPoint
                    , round_ = Rounds.toIntRound round
                    , returnPoint = ExString.toInt returnPoint
                    }
                    |> Rounds.toStringRound
                    |> Rounds.getPoints

            else
                Rounds.getPoints round

        viewShowPointCell_ =
            if not <| Rounds.isDoneInput round then
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
                (Chips.toList chips)
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
                        (players |> Players.toArray |> Array.toList)
                    )
                , tr [ class "editLog_tr" ]
                    (List.indexedMap
                        (\index_ point -> viewInputPointCell roundIndex index_ point)
                        ((Rounds.unwrapRound >> .points >> Array.toList) round)
                    )
                , div [] [ text "トビ賞" ]
                , tr [ class "editLog_tr" ]
                    (List.indexedMap
                        (\index_ tobisho -> viewInputTobisho roundIndex index_ tobisho)
                        ((Rounds.unwrapRound >> .tobisho >> Array.toList) round)
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


{-| 点数入力マス
-}
viewInputTobisho : Int -> Int -> Point -> Html Msg
viewInputTobisho roundIndex playerIndex tobisho =
    td
        [ class "editLog_td" ]
        [ input
            [ class "editLog_inputCellInput"
            , value tobisho
            , onInput <| ChangedTobisho roundIndex playerIndex
            , type_ "number"

            -- , pattern "[0-9]*" -- とすると SP で "-" を入力できないので仕方なく pattern を指定していない。
            -- pattern "[0-9]*" として "+" "-" を入力するボタンを設置するのが今のところ考え得る最善策
            ]
            []
        ]



-- Subs


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ fetchedLog4 FetchedLog4
        , listenedLog4 ListenedLog4
        , fetchedLogButNoLog4 FetchedLogButNoLog4
        , fetchedLog5 FetchedLog5
        , listenedLog5 ListenedLog5
        , fetchedLogButNoLog5 FetchedLogButNoLog5
        ]



-- Functions for Ports


updateLog : LogId -> Log -> Cmd msg
updateLog logId log =
    if Rounds.isRounds4 log.rounds then
        updateLog4 <| Log.toLogDto4 logId log

    else if Rounds.isRounds5 log.rounds then
        updateLog5 <| Log.toLogDto5 logId log

    else
        Cmd.none



-- Ports


port updateLog4 : LogDto4 -> Cmd msg


port updateLog5 : LogDto5 -> Cmd msg


port fetchLog4 : String -> Cmd msg


port fetchLog5 : String -> Cmd msg


port fetchedLog4 : (LogDto4 -> msg) -> Sub msg


port fetchedLog5 : (LogDto5 -> msg) -> Sub msg


port fetchedLogButNoLog4 : (() -> msg) -> Sub msg


port fetchedLogButNoLog5 : (() -> msg) -> Sub msg


port listenLog4 : String -> Cmd msg


port listenLog5 : String -> Cmd msg


port listenedLog4 : (LogDto4 -> msg) -> Sub msg


port listenedLog5 : (LogDto5 -> msg) -> Sub msg
