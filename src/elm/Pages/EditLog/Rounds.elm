module Pages.EditLog.Rounds exposing
    ( IntRound
    , Kaze(..)
    , Point
    , Round
    , Rounds
    , SeatingOrder
    , addRankPointPoints
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
    , initRounds4
    , initRounds5
    , isDefaultRound
    , isDoneInput
    , isRadioButtonChecked
    , isRounds4
    , isRounds5
    , kazeToSelecter
    , kazeToString
    , needsSeatingOrderInput
    , returnedPoints
    , round4FromDto
    , round5FromDto
    , roundFirstDegit
    , sortedPoints
    , test__intRound4
    , toIntRound
    , toRound4Dto
    , toRound5Dto
    , toScores
    , toStringRound
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
import Pages.EditLog.Chips as Chips exposing (Chips)
import Pages.EditLog.Dtos.LogDto exposing (Round4DtoValue, Round5DtoValue, RoundDto(..))
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
    , tobisho : StaticArray Index.Five Point
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
        , tobiSho : StaticArray Index.Five Int
        }



-- functions


isDefaultRound : Round -> Bool
isDefaultRound round =
    case round of
        Round4 _ ->
            round == initRound4

        Round5 _ ->
            round == initRound5


initTobisho4 : StaticArray Index.Four Point
initTobisho4 =
    StaticArray.initialize Length.four (always "")


initTobisho5 : StaticArray Index.Five Point
initTobisho5 =
    StaticArray.initialize Length.five (always "")


initPoints4 : StaticArray Index.Four Point
initPoints4 =
    StaticArray.initialize Length.four (always "")


initPoints5 : StaticArray Index.Five Point
initPoints5 =
    StaticArray.initialize Length.five (always "")


initRound4 : Round
initRound4 =
    Round4
        { points = initPoints4
        , seatingOrder = Nothing
        , tobisho = initTobisho4
        }


initRound5 : Round
initRound5 =
    Round5
        { points = initPoints5
        , seatingOrder = Nothing
        , tobisho = initTobisho5
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


initRounds4 : Rounds
initRounds4 =
    Array.initialize 4 (\_ -> initRound4)


initRounds5 : Rounds
initRounds5 =
    Array.initialize 5 (\_ -> initRound5)


isRounds4 : Rounds -> Bool
isRounds4 rounds =
    case Array.get 0 rounds of
        Just (Round4 _) ->
            True

        Just (Round5 _) ->
            False

        Nothing ->
            False


isRounds5 : Rounds -> Bool
isRounds5 rounds =
    case Array.get 0 rounds of
        Just (Round5 _) ->
            True

        Just (Round4 _) ->
            False

        Nothing ->
            False



-- For Test


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

        IntRound5 { seatingOrder, points, tobiSho } ->
            Round5
                { seatingOrder = seatingOrder
                , points = StaticArray.map String.fromInt points
                , tobisho = StaticArray.map String.fromInt tobiSho
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

        Round5 { seatingOrder, points, tobisho } ->
            IntRound5
                { seatingOrder = seatingOrder
                , points = StaticArray.map (\point -> Maybe.withDefault 0 (String.toInt point)) points
                , tobiSho = StaticArray.map (\tobisho_ -> Maybe.withDefault 0 (String.toInt tobisho_)) tobisho
                }


toRound4Dto : Round -> Round4DtoValue
toRound4Dto round =
    case round of
        Round4 { points, seatingOrder, tobisho } ->
            let
                pointsInt =
                    StaticArray.map ExString.toInt points

                tobiShoInt =
                    StaticArray.map ExString.toInt tobisho
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

        -- 呼ばれないべき branch なので設計がよくなさそう・・・
        Round5 { points, seatingOrder, tobisho } ->
            let
                pointsInt =
                    StaticArray.map ExString.toInt points

                tobiShoInt =
                    StaticArray.map ExString.toInt tobisho
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.five 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.five 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.five 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.five 3) pointsInt
                }
            , tobiSho =
                { data0 = StaticArray.get (Index.fromModBy Length.five 0) tobiShoInt
                , data1 = StaticArray.get (Index.fromModBy Length.five 1) tobiShoInt
                , data2 = StaticArray.get (Index.fromModBy Length.five 2) tobiShoInt
                , data3 = StaticArray.get (Index.fromModBy Length.five 3) tobiShoInt
                }
            }


toRound5Dto : Round -> Round5DtoValue
toRound5Dto round =
    case round of
        Round4 { points, seatingOrder, tobisho } ->
            let
                pointsInt =
                    StaticArray.map ExString.toInt points

                tobiShoInt =
                    StaticArray.map ExString.toInt tobisho
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.four 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.four 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.four 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.four 3) pointsInt
                , data4 = StaticArray.get (Index.fromModBy Length.four 4) pointsInt
                }
            , tobiSho =
                { data0 = StaticArray.get (Index.fromModBy Length.four 0) tobiShoInt
                , data1 = StaticArray.get (Index.fromModBy Length.four 1) tobiShoInt
                , data2 = StaticArray.get (Index.fromModBy Length.four 2) tobiShoInt
                , data3 = StaticArray.get (Index.fromModBy Length.four 3) tobiShoInt
                , data4 = StaticArray.get (Index.fromModBy Length.four 4) tobiShoInt
                }
            }

        -- 呼ばれないべき branch なので設計がよくなさそう・・・
        Round5 { points, seatingOrder, tobisho } ->
            let
                pointsInt =
                    StaticArray.map ExString.toInt points

                tobiShoInt =
                    StaticArray.map ExString.toInt tobisho
            in
            { seatingOrder = seatingOrder
            , points =
                { data0 = StaticArray.get (Index.fromModBy Length.five 0) pointsInt
                , data1 = StaticArray.get (Index.fromModBy Length.five 1) pointsInt
                , data2 = StaticArray.get (Index.fromModBy Length.five 2) pointsInt
                , data3 = StaticArray.get (Index.fromModBy Length.five 3) pointsInt
                , data4 = StaticArray.get (Index.fromModBy Length.five 4) pointsInt
                }
            , tobiSho =
                { data0 = StaticArray.get (Index.fromModBy Length.five 0) tobiShoInt
                , data1 = StaticArray.get (Index.fromModBy Length.five 1) tobiShoInt
                , data2 = StaticArray.get (Index.fromModBy Length.five 2) tobiShoInt
                , data3 = StaticArray.get (Index.fromModBy Length.five 3) tobiShoInt
                , data4 = StaticArray.get (Index.fromModBy Length.five 4) tobiShoInt
                }
            }


getSeatingOrder : Round -> Maybe SeatingOrder
getSeatingOrder round =
    case round of
        Round4 round4 ->
            round4.seatingOrder

        Round5 round5 ->
            round5.seatingOrder


toPoints4FromDto : Round4DtoValue -> StaticArray Index.Four Point
toPoints4FromDto round4DtoValue =
    let
        fromInt : Round4DtoValue -> Int -> String
        fromInt round4Value =
            if
                -- すべて 0 だったら
                [ round4Value.points.data0, round4Value.points.data1, round4Value.points.data2, round4Value.points.data3 ]
                    |> List.filter ((==) 0)
                    |> List.length
                    |> (==) 4
            then
                ExString.fromInt

            else
                String.fromInt
    in
    StaticArray.map
        (fromInt round4DtoValue)
        (StaticArray.fromList
            Length.four
            round4DtoValue.points.data0
            [ round4DtoValue.points.data1, round4DtoValue.points.data2, round4DtoValue.points.data3 ]
        )


toPoints5FromDto : Round5DtoValue -> StaticArray Index.Five Point
toPoints5FromDto round5DtoValue =
    let
        fromInt : Round5DtoValue -> Int -> String
        fromInt round5Value =
            if
                -- すべて 0 だったら
                [ round5Value.points.data0, round5Value.points.data1, round5Value.points.data2, round5Value.points.data3, round5Value.points.data4 ]
                    |> List.filter ((==) 0)
                    |> List.length
                    |> (==) 5
            then
                ExString.fromInt

            else
                String.fromInt
    in
    StaticArray.map
        (fromInt round5DtoValue)
        (StaticArray.fromList
            Length.five
            round5DtoValue.points.data0
            [ round5DtoValue.points.data1, round5DtoValue.points.data2, round5DtoValue.points.data3, round5DtoValue.points.data4 ]
        )


round4FromDto : Round4DtoValue -> Round
round4FromDto round4Value =
    Round4
        { points =
            toPoints4FromDto round4Value
        , seatingOrder = round4Value.seatingOrder
        , tobisho =
            StaticArray.map ExString.fromInt <|
                StaticArray.fromList
                    Length.four
                    round4Value.tobiSho.data0
                    [ round4Value.tobiSho.data1, round4Value.tobiSho.data2, round4Value.tobiSho.data3 ]
        }


round5FromDto : Round5DtoValue -> Round
round5FromDto round5Value =
    Round5
        { points =
            toPoints5FromDto round5Value
        , seatingOrder = round5Value.seatingOrder
        , tobisho =
            StaticArray.map ExString.fromInt <|
                StaticArray.fromList
                    Length.five
                    round5Value.tobiSho.data0
                    [ round5Value.tobiSho.data1, round5Value.tobiSho.data2, round5Value.tobiSho.data3, round5Value.tobiSho.data4 ]
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
                    |> Chips.toArray
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
    { round_ : IntRound

    -- TODO: tuple にするとわかりずらいのでレコードにしよう
    , rankPoint : ( Int, Int )
    , returnPoint : Int
    }


{-| 入力された round, config をもとに順位点を加算した round を返す関数
1着のポイント計算方法を - (2~4着のトータルポイント) としている
-}
calculateRoundFromRawPoint : CalculateRoundFromRawPointConfig -> IntRound
calculateRoundFromRawPoint { round_, rankPoint, returnPoint } =
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
            --
            case List.map2 (\calculatedIntPoint tobisho -> calculatedIntPoint + round (toFloat tobisho / 10)) calculatedIntPoints_ (StaticArray.toList tobiSho) of
                head :: tail ->
                    StaticArray.fromList Length.four head tail

                [] ->
                    points
    in
    case round_ of
        IntRound4 round__ ->
            let
                -- 順位点を加算した Points
                rankPointedPoints__ =
                    round__.points
                        |> returnedPoints returnPoint
                        |> chichaSortedPoints round__.seatingOrder
                        |> sortedPoints
                        -- 五捨六入
                        |> List.map (Tuple.mapSecond roundFirstDegit)
                        -- 10で割る
                        |> List.map (Tuple.mapSecond (\a -> round (toFloat a / 10)))
                        |> addRankPointPoints rankPointArray

                nextPoints =
                    rankPointedPoints__
                        |> totalPointsWithout1st
                        |> calculated1stPointPoints rankPointedPoints__
                        |> calculatedIntPoints
                        |> calculatedIntStaticPoints round__.points round__.tobiSho
            in
            IntRound4
                { round__
                    | points = nextPoints
                }

        IntRound5 round__ ->
            IntRound5 round__



-- TODO: この周辺の関数たち命名なおす


{-| n万点返しした
returnPointは 1000点単位だが、100点単位の入力を受け付けたいため10倍している
-}
returnedPoints : Int -> StaticArray Index.Four Int -> StaticArray Index.Four ( Int, Int )
returnedPoints returnPoint points =
    points
        |> StaticArray.indexedMap
            (\index point -> ( Index.toInt index, point - returnPoint * 10 ))
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



-- a5sha6nyu : List ( Int, Int ) -> List ( Int, Int )
-- a5sha6nyu input =
--     List.map roundFirstDegit input


{-| 3ケタの整数の 1ケタ目を五捨六入 する関数
丸める方の round
-}
roundFirstDegit : Int -> Int
roundFirstDegit arg =
    let
        argMinusFirstDigit =
            (arg
                |> toFloat
                |> (/)
            )
                10
                |> floor
                |> (*) 10

        firstDigit =
            arg - argMinusFirstDigit
    in
    if firstDigit > 5 then
        argMinusFirstDigit + 10

    else
        argMinusFirstDigit


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


{-| Points に順位点を加算
-}
addRankPointPoints : List Int -> List ( Int, Int ) -> List ( Int, ( Int, Int ) )
addRankPointPoints rankPointArray sortedPoints_ =
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


{-| これUI都合なのにここにあっていいのかな
命名がおかしい説もある
needsSeatingOrder でいいかもしれない
-}
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


totalPoint : Rounds -> ( String, String ) -> String -> Stats
totalPoint rounds rankPoint returnPoint =
    let
        calculatedRounds =
            rounds
                |> Array.filter isDoneInput
                |> Array.map
                    (\round ->
                        calculateRoundFromRawPoint
                            { rankPoint = ExTuple.toIntTuple rankPoint
                            , round_ = toIntRound round
                            , returnPoint = ExString.toInt returnPoint
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

        Just (Round5 round5) ->
            Array.set
                roundIndex
                (Round5
                    { round5
                        | points =
                            StaticArray.set
                                (Index.fromModBy Length.five playerIndex)
                                point
                                round5.points
                    }
                )
                rounds

        Nothing ->
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



-- 使われてないかも


filterStaticArray : StaticArray Index.Four String -> StaticArray Index.Four String
filterStaticArray staticArray =
    let
        filteredArray =
            staticArray
                |> StaticArray.toArray
                |> Array.filter ((/=) "")

        head =
            Maybe.withDefault "undefined" <|
                Array.get
                    0
                    filteredArray

        tail =
            filteredArray
                |> Array.slice 1 (Array.length filteredArray)
                |> Array.toList
    in
    StaticArray.fromList
        Length.four
        head
        tail


type alias ToScoresConfig =
    { index : Int
    , rankPoint : ( Int, Int )
    , returnPoint : Int
    , rounds : Rounds
    }


toScores : ToScoresConfig -> Array Int
toScores toScoresConfig =
    toScoresConfig.rounds
        |> Array.map
            (\round ->
                calculateRoundFromRawPoint
                    { round_ = toIntRound round
                    , rankPoint = toScoresConfig.rankPoint
                    , returnPoint = toScoresConfig.returnPoint
                    }
            )
        |> Array.map
            (toScore toScoresConfig.index)


toScore : Int -> IntRound -> Int
toScore index intRound =
    case intRound of
        IntRound4 intRound4 ->
            StaticArray.get (Index.fromModBy Length.four index)
                intRound4.points

        IntRound5 intRound5 ->
            StaticArray.get (Index.fromModBy Length.five index)
                intRound5.points



-- test


test__intRound4 :
    { seatingOrder : Maybe SeatingOrder
    , points : StaticArray Index.Four Int
    , tobiSho : StaticArray Index.Four Int
    }
    -> IntRound
test__intRound4 =
    IntRound4
