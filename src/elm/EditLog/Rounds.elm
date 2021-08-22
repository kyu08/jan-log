module EditLog.Rounds exposing
    ( IntRound
    , Kaze(..)
    , Point
    , Round
    , Rounds
    , SeatingOrder
    , allKazes
    , initRound4
    , initRounds
    , isDefaultPoints
    , kazeToSelecter
    , kazeToString
    , roundInitializer
    , toIntRound
    , toRoundObj4
    , toStringRound
    , toStringRound4
    )

import Array exposing (Array)
import Dtos.LogDto exposing (RoundObj4Dto)
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
