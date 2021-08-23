module EditLog.Rounds exposing
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
    , hasSamePoint
    , initRound4
    , initRounds
    , isDefaultPoints
    , kazeToSelecter
    , kazeToString
    , needsSeatingOrderInput
    , roundInitializer
    , toIntRound
    , toRoundObj4
    , toStringRound
    , toStringRound4
    )

import Array exposing (Array)
import Dtos.LogDto exposing (RoundObj4Dto)
import EditLog.Chips exposing (Chips)
import Expands.Array as ExArray



-- types


type alias Rounds =
    Array Round


{-| 半荘データ
同点の場合は起家を入力して順位点を確定する
chicha: PlayerIndex
-}
type alias Round =
    { points : Array Point
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


type alias IntRound =
    { seatingOrder : Maybe SeatingOrder
    , points : Array Int
    }



-- functions
-- functions for rounds, round


isDefaultPoints : Array Point -> Bool
isDefaultPoints points =
    (points == initRound4.points)
        || (points == Array.initialize 4 (always "0"))


initRound4 : Round
initRound4 =
    { points = Array.initialize 4 (always "")
    , seatingOrder = Nothing
    }


roundInitializer : a -> Round
roundInitializer =
    \_ -> initRound4


{-| TODO: Rounds.elm をつくってファクトリーメソッドをつくる?
-}
initRounds : Rounds
initRounds =
    Array.initialize
        4
        roundInitializer


toStringRound : IntRound -> Round
toStringRound intRound =
    { seatingOrder = intRound.seatingOrder
    , points = Array.map String.fromInt intRound.points
    }


toIntRound : Round -> IntRound
toIntRound { seatingOrder, points } =
    { seatingOrder = seatingOrder
    , points = ExArray.toIntArray points
    }


toStringRound4 : RoundObj4Dto -> Round
toStringRound4 { points, seatingOrder } =
    let
        stringFromInt int =
            if int == 0 then
                ""

            else
                String.fromInt int
    in
    { seatingOrder = seatingOrder
    , points =
        Array.map stringFromInt <|
            Array.fromList
                [ points.data0
                , points.data1
                , points.data2
                , points.data3
                ]
    }


toRoundObj4 : Round -> RoundObj4Dto
toRoundObj4 { seatingOrder, points } =
    let
        pointsInt =
            ExArray.toIntArray points
    in
    { seatingOrder = seatingOrder
    , points =
        { data0 = ExArray.getArrayElement 0 pointsInt
        , data1 = ExArray.getArrayElement 1 pointsInt
        , data2 = ExArray.getArrayElement 2 pointsInt
        , data3 = ExArray.getArrayElement 3 pointsInt
        }
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



-- functions for calculation
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
