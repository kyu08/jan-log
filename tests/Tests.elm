module Tests exposing (..)

import Expect
import Pages.EditLog.Rounds as Rounds
import StaticArray
import StaticArray.Length as Length
import Test exposing (..)


rounds : Test
rounds =
    describe "Rounds.elm"
        [ test "Round4 の順位点を計算する" <|
            let
                expectedValue =
                    Rounds.test__intRound4
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

                testValue =
                    Rounds.calculateRoundFromRawPoint
                        { round =
                            Rounds.test__intRound4
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
                        , rankPoint = ( 10, 20 )
                        , havePoint = 25
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "Round4 の順位点を計算する(同点者がいる場合)" <|
            let
                expectedValue =
                    Rounds.test__intRound4
                        { seatingOrder =
                            Just
                                { ton = 0
                                , nan = 2
                                , sha = 1
                                , pei = 3
                                }
                        , points = StaticArray.fromList Length.four -40 [ -20, 0, 60 ]
                        , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
                        }

                testValue =
                    Rounds.calculateRoundFromRawPoint
                        { round =
                            Rounds.test__intRound4
                                { seatingOrder =
                                    Just
                                        { ton = 0
                                        , nan = 2
                                        , sha = 1
                                        , pei = 3
                                        }
                                , points = StaticArray.fromList Length.four 10 [ 20, 20, 50 ]
                                , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
                                }
                        , rankPoint = ( 10, 20 )
                        , havePoint = 25
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "Round4 の順位点を計算する(トビ賞がある場合)" <|
            let
                expectedValue =
                    Rounds.test__intRound4
                        { seatingOrder =
                            Just
                                { ton = 0
                                , nan = 1
                                , sha = 2
                                , pei = 3
                                }
                        , points = StaticArray.fromList Length.four -50 [ -10, 10, 50 ]
                        , tobiSho = StaticArray.fromList Length.four -10 [ 10, 0, 0 ]
                        }

                testValue =
                    Rounds.calculateRoundFromRawPoint
                        { round =
                            Rounds.test__intRound4
                                { seatingOrder =
                                    Just
                                        { ton = 0
                                        , nan = 1
                                        , sha = 2
                                        , pei = 3
                                        }
                                , points = StaticArray.fromList Length.four 10 [ 20, 30, 40 ]
                                , tobiSho = StaticArray.fromList Length.four -10 [ 10, 0, 0 ]
                                }
                        , rankPoint = ( 10, 20 )
                        , havePoint = 25
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3万点返しする" <|
            let
                expectedValue =
                    StaticArray.fromList Length.four ( 0, -20 ) [ ( 1, -10 ), ( 2, 0 ), ( 3, 10 ) ]

                testValue =
                    Rounds.returnedPoints 30 <|
                        StaticArray.fromList Length.four 10 [ 20, 30, 40 ]
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "座順データが存在すれば起家ソートを行う" <|
            let
                expectedValue =
                    StaticArray.fromList Length.four ( 0, -20 ) [ ( 1, -10 ), ( 2, 0 ), ( 3, 10 ) ]

                testValue =
                    Rounds.chichaSortedPoints
                        (Just { pei = 0, sha = 1, nan = 2, ton = 3 })
                        (StaticArray.fromList
                            Length.four
                            ( 0, -20 )
                            [ ( 1, -10 ), ( 2, 0 ), ( 3, 10 ) ]
                        )
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "point でソート" <|
            let
                expectedValue =
                    [ ( 3, 10 )
                    , ( 2, 0 )
                    , ( 1, -10 )
                    , ( 0, -20 )
                    ]

                testValue =
                    Rounds.sortedPoints
                        (StaticArray.fromList
                            Length.four
                            ( 0, -20 )
                            [ ( 1, -10 ), ( 2, 0 ), ( 3, 10 ) ]
                        )
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "順位点を加算" <|
            let
                expectedValue =
                    [ ( 0, ( 3, 30 ) )
                    , ( 1, ( 2, 10 ) )
                    , ( 2, ( 1, -20 ) )
                    , ( 3, ( 0, -40 ) )
                    ]

                testValue =
                    Rounds.rankPointedPoints
                        [ 20, 10, -10, -20 ]
                        [ ( 3, 10 ), ( 2, 0 ), ( 1, -10 ), ( 0, -20 ) ]
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2着 ~ 3着 のプレイヤーの合計(1着のポイント計算用に使う)" <|
            let
                expectedValue =
                    -50

                testValue =
                    Rounds.totalPointsWithout1st
                        [ ( 0, ( 3, 30 ) )
                        , ( 1, ( 2, 10 ) )
                        , ( 2, ( 1, -20 ) )
                        , ( 3, ( 0, -40 ) )
                        ]
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2 ~ 3 着のポイント合計をマイナスしたものを1着のポイントとして計算する" <|
            let
                expectedValue =
                    [ ( 3, 50 )
                    , ( 2, 10 )
                    , ( 1, -20 )
                    , ( 0, -40 )
                    ]

                testValue =
                    Rounds.calculated1stPointPoints
                        [ ( 0, ( 3, 30 ) )
                        , ( 1, ( 2, 10 ) )
                        , ( 2, ( 1, -20 ) )
                        , ( 3, ( 0, -40 ) )
                        ]
                        -50
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "4桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    3550

                testValue =
                    Rounds.roundPoint 3555
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "4桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    2020

                testValue =
                    Rounds.roundPoint 2019
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    350

                testValue =
                    Rounds.roundPoint 355
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    360

                testValue =
                    Rounds.roundPoint 356
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    10

                testValue =
                    Rounds.roundPoint 13
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    40

                testValue =
                    Rounds.roundPoint 39
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    0

                testValue =
                    Rounds.roundPoint 2
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    10

                testValue =
                    Rounds.roundPoint 7
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること3" <|
            let
                expectedValue =
                    0

                testValue =
                    Rounds.roundPoint 0
            in
            \_ ->
                Expect.equal expectedValue testValue
        ]
