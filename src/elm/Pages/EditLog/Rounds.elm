module Pages.EditLog.Rounds exposing
    ( IntRound
    , Kaze(..)
    , Point
    , Round
    , Rounds
    , SeatingOrder
    , allKazes
    , calculateFrom2Arrays
    , calculateRoundFromRawPoint
    , calculateTotalBalanceExcludeGameFee
    , calculateTotalBalanceIncludeGameFee
    , calculateTotalPoint
    , calculateTotalPointIncludeChip
    , getPoints
    , getSeatingOrderInput
    , hasSamePoint
    , initPoint
    , initRound4
    , initRounds
    , isDefaultPoints
    , isRadioButtonChecked
    , kazeToSelecter
    , kazeToString
    , needsSeatingOrderInput
    , roundFromDto
    , roundInitializer
    , toIntRound
    , toRoundObj4
    , toStringRound
    , toStringRound4
    , totalPoint
    , unwrapRound
    , updatePoints
    , updateSeatingOrder
    )

import Array exposing (Array)
import Expands.Array as ExArray
import Expands.String as ExString
import Expands.Tuple as ExTuple
import Pages.EditLog.Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (RoundObj4Dto)
import Pages.EditLog.SeatingOrderInput exposing (SeatingOrderInput)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Four)
import StaticArray.Length as Length



-- types


type alias Rounds =
    Array Round


{-| 半荘データ
同点の場合は起家を入力して順位点を確定する
chicha: PlayerIndex
-}
type Round
    = Round4
        { points : StaticArray Four Point
        , seatingOrder : Maybe SeatingOrder
        }


type alias Point =
    String


type alias SeatingOrder =
    { ton : Int
    , nan : Int
    , sha : Int
    , pei : Int
    }


type IntRound
    = IntRound4
        { seatingOrder : Maybe SeatingOrder
        , points : StaticArray Four Int
        }



-- functions


isDefaultPoints : Round -> Bool
isDefaultPoints round =
    case round of
        Round4 _ ->
            round == initRound4


initPoints4 : StaticArray Four Point
initPoints4 =
    StaticArray.initialize Length.four (always "")


initRound4 : Round
initRound4 =
    Round4
        { points = initPoints4
        , seatingOrder = Nothing
        }


unwrapRound : Round -> { points : Array String, seatingOrder : Maybe SeatingOrder }
unwrapRound round =
    case round of
        Round4 round4 ->
            { points = StaticArray.toArray round4.points
            , seatingOrder = round4.seatingOrder
            }


initPoint : Round -> Array String
initPoint round =
    case round of
        Round4 _ ->
            initRound4
                |> unwrapRound
                |> .points


roundInitializer : a -> Round
roundInitializer =
    \_ -> initRound4


{-| TODO: validation とかどうしよう
-}
initRounds : Rounds
initRounds =
    Array.initialize 4 roundInitializer


isDoneInput : Round -> Bool
isDoneInput round =
    case round of
        -- 入力済みのポイントが4つ以上ある -> 入力済みとみなす
        Round4 round4 ->
            round4.points
                |> StaticArray.toList
                |> List.filter ((/=) "")
                |> List.length
                |> (<=) 4


toStringRound : IntRound -> Round
toStringRound intRound =
    case intRound of
        IntRound4 { seatingOrder, points } ->
            Round4
                { seatingOrder = seatingOrder
                , points = StaticArray.map String.fromInt points
                }


toIntRound : Round -> IntRound
toIntRound round =
    case round of
        Round4 { seatingOrder, points } ->
            IntRound4
                { seatingOrder = seatingOrder
                , points = StaticArray.map (\point -> Maybe.withDefault 0 (String.toInt point)) points
                }


toStringRound4 : RoundObj4Dto -> Round
toStringRound4 { points, seatingOrder } =
    let
        points_ =
            case List.map ExString.fromInt <| [ points.data0, points.data1, points.data2, points.data3 ] of
                head :: tail ->
                    StaticArray.fromList Length.four head tail

                [] ->
                    initPoints4
    in
    Round4
        { seatingOrder = seatingOrder
        , points = points_
        }


toRoundObj4 : Round -> RoundObj4Dto
toRoundObj4 round =
    case round of
        Round4 { points, seatingOrder } ->
            let
                pointsInt =
                    StaticArray.map ExString.toIntValue points
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.four 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.four 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.four 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.four 3) pointsInt
                }
            }


roundFromDto : RoundObj4Dto -> Round
roundFromDto { points, seatingOrder } =
    let
        points_ =
            StaticArray.map ExString.fromInt <|
                StaticArray.fromList
                    Length.four
                    points.data0
                    [ points.data1, points.data2, points.data3 ]
    in
    Round4
        { points = points_
        , seatingOrder = seatingOrder
        }



-- functions for seatingOrder


type Kaze
    = Ton
    | Nan
    | Sha
    | Pei


allKazes : List Kaze
allKazes =
    [ Ton, Nan, Sha, Pei ]


kazeToString : Kaze -> String
kazeToString kaze =
    case kaze of
        Ton ->
            "東家"

        Nan ->
            "南家"

        Sha ->
            "西家"

        Pei ->
            "北家"


kazeToSelecter : Kaze -> { a | ton : b, nan : b, sha : b, pei : b } -> b
kazeToSelecter kaze =
    case kaze of
        Ton ->
            .ton

        Nan ->
            .nan

        Sha ->
            .sha

        Pei ->
            .pei


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
        (Array.initialize 1
            (\_ ->
                chips
                    |> StaticArray.toArray
                    |> ExArray.toIntArray
            )
        )


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


{-| 入力された round, config をもとに順位点を加算した round を返す関数
トビを考慮するために1着のポイント計算方法を - (2~4着のトータルポイント) としている
トビは現状の実装だと場外で(チップなどで)やりとりするしかないので微妙ではある
-}
calculateRoundFromRawPoint : CalculateRoundFromRawPointConfig -> IntRound
calculateRoundFromRawPoint { round, rankPoint, havePoint, returnPoint } =
    case round of
        IntRound4 { points, seatingOrder } ->
            let
                -- 順位点が入ってる List
                rankPointArray =
                    [ Tuple.second rankPoint
                    , Tuple.first rankPoint
                    , negate <| Tuple.first rankPoint
                    , negate <| Tuple.second rankPoint
                    ]

                -- n万点返しした
                returnedPoints =
                    StaticArray.indexedMap
                        (\index point -> ( Index.toInt index, point - returnPoint ))
                        points

                -- 座順データが存在すれば起家ソートを行う
                chichaSortedPoints =
                    case seatingOrder of
                        Just seatingOrder_ ->
                            StaticArray.fromList Length.four
                                (StaticArray.get (Index.fromModBy Length.four seatingOrder_.pei) returnedPoints)
                                -- (StaticArray.get (Index.fromModBy Length.four seatingOrder_.pei) (Debug.log "returnedPoints---------------------------" returnedPoints))
                                [ StaticArray.get (Index.fromModBy Length.four seatingOrder_.sha) returnedPoints
                                , StaticArray.get (Index.fromModBy Length.four seatingOrder_.nan) returnedPoints
                                , StaticArray.get (Index.fromModBy Length.four seatingOrder_.ton) returnedPoints
                                ]

                        Nothing ->
                            returnedPoints

                -- point でソート
                sortedPoints =
                    List.reverse <|
                        List.sortBy
                            Tuple.second
                            (StaticArray.toList chichaSortedPoints)

                -- 順位点を加算
                rankPointedPoints =
                    List.map2
                        (\rankPoint_ ( rank, ( index, point ) ) ->
                            ( rank, ( index, point + rankPoint_ ) )
                        )
                        rankPointArray
                        (List.indexedMap (\rank roundWithIndex -> ( rank, roundWithIndex )) sortedPoints)

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
                        rankPointedPoints

                -- 2 ~ 3 着のポイント合計をマイナスしたものを1着のポイントとして計算する
                calculated1stPointPoints =
                    List.map
                        (\( rank, ( index, point ) ) ->
                            if rank == 0 then
                                ( index, negate totalPointsWithout1st )

                            else
                                ( index, point )
                        )
                        rankPointedPoints

                calculatedIntPoints =
                    calculated1stPointPoints
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second

                calculatedIntStaticPoints =
                    case calculatedIntPoints of
                        head :: tail ->
                            StaticArray.fromList Length.four head tail

                        [] ->
                            points
            in
            IntRound4
                { seatingOrder = seatingOrder
                , points = calculatedIntStaticPoints
                }


needsSeatingOrderInput : Round -> Bool
needsSeatingOrderInput round =
    let
        points =
            (unwrapRound >> .points) round
    in
    -- 入力が完了していない場合は起家の入力を求めない
    if not <| isDoneInput round then
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


totalPoint : Rounds -> ( String, String ) -> String -> String -> Stats
totalPoint rounds rankPoint havePoint returnPoint =
    let
        calculatedRounds =
            rounds
                |> Array.map
                    (\round ->
                        if not <| isDefaultPoints round then
                            calculateRoundFromRawPoint
                                { rankPoint = ExTuple.toIntTuple rankPoint
                                , round = toIntRound round
                                , havePoint = ExString.toIntValue havePoint
                                , returnPoint = ExString.toIntValue returnPoint
                                }

                        else
                            toIntRound round
                    )
    in
    calculatedRounds
        |> Array.map
            (\round ->
                case round of
                    IntRound4 round4 ->
                        StaticArray.toArray round4.points
            )
        |> calculateTotalPoint


type alias UpdatePointConfig =
    { point : Point
    , rounds : Rounds
    , roundIndex : Int
    , playerIndex : Int
    }


updatePoints : UpdatePointConfig -> Rounds
updatePoints { point, rounds, roundIndex, playerIndex } =
    case Array.get roundIndex rounds of
        Just (Round4 round4) ->
            Array.set
                roundIndex
                (Round4
                    { round4
                        | points =
                            StaticArray.set
                                (Index.fromModBy Length.four playerIndex)
                                point
                                round4.points
                    }
                )
                rounds

        _ ->
            rounds


type alias UpdateRoundsConfig =
    { roundIndex : Int
    , rounds : Rounds
    , round : Round
    }


getSeatingOrderInput : Round -> Maybe SeatingOrderInput
getSeatingOrderInput round =
    case round of
        Round4 round4 ->
            Maybe.andThen
                (\seatingOrder ->
                    Just
                        { ton = Just seatingOrder.ton
                        , nan = Just seatingOrder.nan
                        , sha = Just seatingOrder.sha
                        , pei = Just seatingOrder.pei
                        }
                )
                round4.seatingOrder


updateSeatingOrder : SeatingOrderInput -> Round -> Round
updateSeatingOrder seatingOrderInput round =
    case round of
        Round4 round4 ->
            Round4 { round4 | seatingOrder = toSeatingOrder seatingOrderInput }


toSeatingOrder : SeatingOrderInput -> Maybe SeatingOrder
toSeatingOrder { ton, nan, sha, pei } =
    Maybe.map4
        (\ton_ nan_ sha_ pei_ ->
            { ton = ton_
            , nan = nan_
            , sha = sha_
            , pei = pei_
            }
        )
        ton
        nan
        sha
        pei


getPoints : Round -> Array String
getPoints round =
    case round of
        Round4 round4 ->
            StaticArray.toArray round4.points


isRadioButtonChecked : Round -> Kaze -> Int -> SeatingOrderInput -> Bool
isRadioButtonChecked round kaze playerIndex seatingOrderInput =
    case round of
        Round4 round4 ->
            case round4.seatingOrder of
                Just seatingOrder ->
                    kazeToSelecter kaze seatingOrder == playerIndex

                Nothing ->
                    case kazeToSelecter kaze seatingOrderInput of
                        Just playerIndex_ ->
                            playerIndex == playerIndex_

                        Nothing ->
                            False
