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
    , calculated1stPointPoints
    , chichaSortedPoints
    , getPoints
    , getSeatingOrderInput
    , hasSamePoint
    , initPoint
    , initRound4
    , initRounds
    , isDefaultPoints
    , isDoneInput
    , isRadioButtonChecked
    , kazeToSelecter
    , kazeToString
    , needsSeatingOrderInput
    , rankPointedPoints
    , returnedPoints
    , roundFromDto
    , sortedPoints
    , test__intRound4
    , test__intRound4ForTesting1
    , test__intRound4ForTesting1Expected
    , toIntRound
    , toRoundObj4
    , toStringRound
    , toStringRound4
    , totalPoint
    , totalPointsWithout1st
    , unwrapRound
    , updatePoints
    , updateSeatingOrder
    , updateTobisho
    )

import Array exposing (Array)
import Expands.Array as ExArray
import Expands.String as ExString
import Expands.Tuple as ExTuple
import Pages.EditLog.Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (Round4Dto)
import Pages.EditLog.SeatingOrderInput exposing (SeatingOrderInput)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index
import StaticArray.Length as Length



-- types


type alias Rounds =
    Array Round


{-| 半荘データ
同点の場合は起家を入力して順位点を確定する
chicha: PlayerIndex
-}
type Round
    = Round4 Round4Value
    | Round5 Round5Value


type alias Round4Value =
    { points : StaticArray Index.Four Point
    , seatingOrder : Maybe SeatingOrder
    , tobisho : StaticArray Index.Four Point
    }


type alias Round5Value =
    { points : StaticArray Index.Five Point
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
        { points : StaticArray Index.Four Int
        , seatingOrder : Maybe SeatingOrder
        , tobiSho : StaticArray Index.Four Int
        }
    | IntRound5
        { points : StaticArray Index.Five Int
        , seatingOrder : Maybe SeatingOrder
        }



-- functions


isDefaultPoints : Round -> Bool
isDefaultPoints round =
    case round of
        Round4 _ ->
            round == initRound4

        Round5 _ ->
            round == initRound5


initTobisho4 : StaticArray Index.Four Point
initTobisho4 =
    StaticArray.initialize Length.four (always "")


initPoints4 : StaticArray Index.Four Point
initPoints4 =
    StaticArray.initialize Length.four (always "")


initPoints5 : StaticArray Index.Five Point
initPoints5 =
    StaticArray.initialize Length.five (always "")


initRound4 : Round
initRound4 =
    Round4
        { points = initTobisho4
        , seatingOrder = Nothing
        , tobisho = initTobisho4
        }


initRound5 : Round
initRound5 =
    Round5
        { points = initPoints5
        , seatingOrder = Nothing
        }


unwrapRound : Round -> { points : Array String, seatingOrder : Maybe SeatingOrder, tobisho : Array String }
unwrapRound round =
    case round of
        Round4 round4 ->
            { points = StaticArray.toArray round4.points
            , seatingOrder = round4.seatingOrder
            , tobisho = StaticArray.toArray round4.tobisho
            }

        Round5 round5 ->
            { points = StaticArray.toArray round5.points
            , seatingOrder = round5.seatingOrder
            , tobisho = Array.fromList [ "" ] -- FIXME
            }


initPoint : Round -> Array String
initPoint round =
    case round of
        Round4 _ ->
            initRound4
                |> unwrapRound
                |> .points

        Round5 _ ->
            initRound5
                |> unwrapRound
                |> .points


{-| TODO: validation とかどうしよう
-}
initRounds : Rounds
initRounds =
    Array.initialize 4 (\_ -> initRound4)



-- For Test


test__intRound4ForTesting1 : IntRound
test__intRound4ForTesting1 =
    IntRound4
        { seatingOrder =
            Just
                { ton = 0
                , nan = 1
                , sha = 2
                , pei = 3
                }
        , points = StaticArray.fromList Length.four 10 [ 20, 30, 40 ]
        , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
        }


{-| TODO: テスト用の値がここにあるのは不適切なのでバリアントだけ expose するなどする。
-}
test__intRound4ForTesting1Expected : IntRound
test__intRound4ForTesting1Expected =
    IntRound4
        { seatingOrder =
            Just
                { ton = 0
                , nan = 1
                , sha = 2
                , pei = 3
                }
        , points = StaticArray.fromList Length.four -40 [ -20, 10, 50 ]
        , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
        }


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

        Round5 round5 ->
            round5.points
                |> StaticArray.toList
                |> List.filter ((/=) "")
                |> List.length
                |> (<=) 4


toStringRound : IntRound -> Round
toStringRound intRound =
    case intRound of
        IntRound4 { seatingOrder, points, tobiSho } ->
            Round4
                { seatingOrder = seatingOrder
                , points = StaticArray.map String.fromInt points
                , tobisho = StaticArray.map String.fromInt tobiSho
                }

        IntRound5 { seatingOrder, points } ->
            Round5
                { seatingOrder = seatingOrder
                , points = StaticArray.map String.fromInt points
                }


toIntRound : Round -> IntRound
toIntRound round =
    case round of
        Round4 { seatingOrder, points, tobisho } ->
            IntRound4
                { seatingOrder = seatingOrder
                , points = StaticArray.map (\point -> Maybe.withDefault 0 (String.toInt point)) points
                , tobiSho = StaticArray.map (\tobisho_ -> Maybe.withDefault 0 (String.toInt tobisho_)) tobisho
                }

        Round5 { seatingOrder, points } ->
            IntRound5
                { seatingOrder = seatingOrder
                , points = StaticArray.map (\point -> Maybe.withDefault 0 (String.toInt point)) points
                }


toStringRound4 : Round4Dto -> Round
toStringRound4 { points, seatingOrder, tobiSho } =
    let
        points_ =
            case List.map ExString.fromInt <| [ points.data0, points.data1, points.data2, points.data3 ] of
                head :: tail ->
                    StaticArray.fromList Length.four head tail

                [] ->
                    initTobisho4

        tobisho_ =
            case List.map ExString.fromInt <| [ tobiSho.data0, tobiSho.data1, tobiSho.data2, tobiSho.data3 ] of
                head :: tail ->
                    StaticArray.fromList Length.four head tail

                [] ->
                    initTobisho4
    in
    Round4
        { seatingOrder = seatingOrder
        , points = points_
        , tobisho = tobisho_
        }


toRoundObj4 : Round -> Round4Dto
toRoundObj4 round =
    case round of
        Round4 { points, seatingOrder, tobisho } ->
            let
                pointsInt =
                    StaticArray.map ExString.toIntValue points

                tobiShoInt =
                    StaticArray.map ExString.toIntValue tobisho
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.four 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.four 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.four 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.four 3) pointsInt
                }
            , tobiSho =
                { data0 = StaticArray.get (Index.fromModBy Length.four 0) tobiShoInt
                , data1 = StaticArray.get (Index.fromModBy Length.four 1) tobiShoInt
                , data2 = StaticArray.get (Index.fromModBy Length.four 2) tobiShoInt
                , data3 = StaticArray.get (Index.fromModBy Length.four 3) tobiShoInt
                }
            }

        Round5 { points, seatingOrder } ->
            let
                pointsInt =
                    StaticArray.map ExString.toIntValue points
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.five 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.five 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.five 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.five 3) pointsInt
                }
            , tobiSho =
                { data0 = 0
                , data1 = 0
                , data2 = 0
                , data3 = 0
                }
            }


getSeatingOrder : Round -> Maybe SeatingOrder
getSeatingOrder round =
    case round of
        Round4 round4 ->
            round4.seatingOrder

        Round5 round5 ->
            round5.seatingOrder


roundFromDto : Round4Dto -> Round
roundFromDto { points, seatingOrder, tobiSho } =
    let
        points_ =
            StaticArray.map ExString.fromInt <|
                StaticArray.fromList
                    Length.four
                    points.data0
                    [ points.data1, points.data2, points.data3 ]

        tobiSho_ =
            StaticArray.map ExString.fromInt <|
                StaticArray.fromList
                    Length.four
                    tobiSho.data0
                    [ tobiSho.data1, tobiSho.data2, tobiSho.data3 ]
    in
    Round4
        { points = points_
        , seatingOrder = seatingOrder
        , tobisho = tobiSho_
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

    -- TODO: tuple にするとわかりずらいのでレコードにしよう
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
    let
        -- 順位点が入ってる List
        rankPointArray =
            [ Tuple.second rankPoint
            , Tuple.first rankPoint
            , negate <| Tuple.first rankPoint
            , negate <| Tuple.second rankPoint
            ]

        calculatedIntPoints calculated1stPointPoints_ =
            calculated1stPointPoints_
                |> List.sortBy Tuple.first
                |> List.map Tuple.second

        -- トビ賞を計算して StaticArray に戻す
        calculatedIntStaticPoints points tobiSho calculatedIntPoints_ =
            case List.map2 (+) calculatedIntPoints_ (StaticArray.toList tobiSho) of
                head :: tail ->
                    StaticArray.fromList Length.four head tail

                [] ->
                    points
    in
    case round of
        IntRound4 round_ ->
            let
                rankPointedPoints__ =
                    round_.points
                        |> returnedPoints returnPoint
                        |> chichaSortedPoints round_.seatingOrder
                        |> sortedPoints
                        |> rankPointedPoints rankPointArray

                nextPoints =
                    rankPointedPoints__
                        |> totalPointsWithout1st
                        |> calculated1stPointPoints rankPointedPoints__
                        |> calculatedIntPoints
                        |> calculatedIntStaticPoints round_.points round_.tobiSho
            in
            IntRound4
                { round_
                    | points = nextPoints
                }

        IntRound5 { points, seatingOrder } ->
            IntRound5
                { points = points
                , seatingOrder = seatingOrder
                }



-- TODO: この周辺の関数たち命名なおす


{-| n万点返しした
-}
returnedPoints : Int -> StaticArray Index.Four Int -> StaticArray Index.Four ( Int, Int )
returnedPoints returnPoint points =
    points
        |> StaticArray.indexedMap
            (\index point -> ( Index.toInt index, point - returnPoint ))
        |> StaticArray.toList
        |> (\points_ ->
                case points_ of
                    head :: tail ->
                        StaticArray.fromList Length.four
                            head
                            (List.map
                                -- StaticArray.indexedMap だと index がうまくとれないので +2 している
                                (\( a, b ) -> ( a + 2, b ))
                                tail
                            )

                    [] ->
                        points
                            |> StaticArray.indexedMap
                                (\index point -> ( Index.toInt index, point - returnPoint ))
           )


{-| 座順データが存在すれば起家ソートを行う
-}
chichaSortedPoints : Maybe { a | pei : Int, sha : Int, nan : Int, ton : Int } -> StaticArray Index.Four ( Int, Int ) -> StaticArray Index.Four ( Int, Int )
chichaSortedPoints seatingOrder returnedPoints_ =
    case seatingOrder of
        Just seatingOrder_ ->
            StaticArray.fromList Length.four
                (StaticArray.get (Index.fromModBy Length.four seatingOrder_.pei) returnedPoints_)
                [ StaticArray.get (Index.fromModBy Length.four seatingOrder_.sha) returnedPoints_
                , StaticArray.get (Index.fromModBy Length.four seatingOrder_.nan) returnedPoints_
                , StaticArray.get (Index.fromModBy Length.four seatingOrder_.ton) returnedPoints_
                ]

        Nothing ->
            returnedPoints_


{-| point でソート
-}
sortedPoints : StaticArray Index.Four ( Int, Int ) -> List ( Int, Int )
sortedPoints chichaSortedPoints_ =
    List.reverse <|
        List.sortBy
            Tuple.second
            (StaticArray.toList chichaSortedPoints_)


{-| 順位点を加算
-}
rankPointedPoints : List Int -> List ( Int, Int ) -> List ( Int, ( Int, Int ) )
rankPointedPoints rankPointArray sortedPoints_ =
    List.map2
        (\rankPoint_ ( rank, ( index, point ) ) ->
            ( rank, ( index, point + rankPoint_ ) )
        )
        rankPointArray
        (List.indexedMap (\rank roundWithIndex -> ( rank, roundWithIndex )) sortedPoints_)


{-| 2着 ~ 3着 のプレイヤーの合計(1着のポイント計算用に使う)
-}
totalPointsWithout1st : List ( Int, ( Int, Int ) ) -> Int
totalPointsWithout1st rankPointedPoints_ =
    List.foldl
        (\( rank, ( _, point ) ) acumulator ->
            if rank == 0 then
                acumulator

            else
                point + acumulator
        )
        0
        rankPointedPoints_


{-| 2 ~ 3 着のポイント合計をマイナスしたものを1着のポイントとして計算する
-}
calculated1stPointPoints : List ( Int, ( Int, Int ) ) -> Int -> List ( Int, Int )
calculated1stPointPoints rankPointedPoints_ totalPointsWithout1st_ =
    List.map
        (\( rank, ( index, point ) ) ->
            if rank == 0 then
                ( index, negate totalPointsWithout1st_ )

            else
                ( index, point )
        )
        rankPointedPoints_


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
                |> Array.filter isDoneInput
                |> Array.map
                    (\round ->
                        calculateRoundFromRawPoint
                            { rankPoint = ExTuple.toIntTuple rankPoint
                            , round = toIntRound round
                            , havePoint = ExString.toIntValue havePoint
                            , returnPoint = ExString.toIntValue returnPoint
                            }
                    )
    in
    calculatedRounds
        |> Array.map
            (\round ->
                case round of
                    IntRound4 round4 ->
                        StaticArray.toArray round4.points

                    IntRound5 round5 ->
                        StaticArray.toArray round5.points
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


type alias UpdateTobishoConfig =
    { tobisho : Point
    , rounds : Rounds
    , roundIndex : Int
    , playerIndex : Int
    }


updateTobisho : UpdateTobishoConfig -> Rounds
updateTobisho { tobisho, rounds, roundIndex, playerIndex } =
    case Array.get roundIndex rounds of
        Just (Round4 round4) ->
            Array.set
                roundIndex
                (Round4
                    { round4
                        | tobisho =
                            StaticArray.set
                                (Index.fromModBy Length.four playerIndex)
                                tobisho
                                round4.tobisho
                    }
                )
                rounds

        _ ->
            rounds


getSeatingOrderInput : Round -> Maybe SeatingOrderInput
getSeatingOrderInput round =
    Maybe.andThen
        (\seatingOrder ->
            Just
                { ton = Just seatingOrder.ton
                , nan = Just seatingOrder.nan
                , sha = Just seatingOrder.sha
                , pei = Just seatingOrder.pei
                }
        )
        (getSeatingOrder round)


{-| TODO: update でやる
-}
updateSeatingOrder : SeatingOrderInput -> Round -> Round
updateSeatingOrder seatingOrderInput round =
    case round of
        Round4 round4 ->
            Round4 { round4 | seatingOrder = toSeatingOrder seatingOrderInput }

        Round5 round5 ->
            Round5 { round5 | seatingOrder = toSeatingOrder seatingOrderInput }


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

        Round5 round5 ->
            StaticArray.toArray round5.points


isRadioButtonChecked : Round -> Kaze -> Int -> SeatingOrderInput -> Bool
isRadioButtonChecked round kaze playerIndex seatingOrderInput =
    case getSeatingOrder round of
        Just seatingOrder ->
            kazeToSelecter kaze seatingOrder == playerIndex

        Nothing ->
            case kazeToSelecter kaze seatingOrderInput of
                Just playerIndex_ ->
                    playerIndex == playerIndex_

                Nothing ->
                    False



-- test


test__intRound4 :
    { seatingOrder : Maybe SeatingOrder
    , points : StaticArray Index.Four Int
    , tobiSho : StaticArray Index.Four Int
    }
    -> IntRound
test__intRound4 =
    IntRound4
