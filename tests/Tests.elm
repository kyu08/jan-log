module Tests exposing (..)

import Array
import Expands.Time exposing (posixToYmdWithHyphenDelimiter)
import Expect
import IO.Miyabq as Miyabq
import Json.Encode as E
import Pages.EditLog.Log as Log
import Pages.EditLog.Rounds as Rounds
import StaticArray
import StaticArray.Length as Length
import Test exposing (..)
import Time


jsonEncode : Test
jsonEncode =
    describe "Miyabq.elm"
        [ test "json Encode" <|
            let
                expectedValue =
                    "{\"match_date\":\"2021-10-11\",\"results\":[{\"user_id\":1,\"scores\":[1,2,3,4,5,67,3,-10],\"chip\":10},{\"user_id\":2,\"scores\":[-1,-2,-3,-4,-5,-67,-3,-10],\"chip\":11},{\"user_id\":3,\"scores\":[10,20,30,40,50,670,30,-100],\"chip\":12},{\"user_id\":4,\"scores\":[11,21,31,41,51,671,31,-110],\"chip\":13}]}"

                resultDto =
                    { match_date = "2021-10-11"
                    , results =
                        [ { user_id = 1
                          , scores = Array.fromList [ 1, 2, 3, 4, 5, 67, 3, -10 ]
                          , chip = 10
                          }
                        , { user_id = 2
                          , scores = Array.fromList [ -1, -2, -3, -4, -5, -67, -3, -10 ]
                          , chip = 11
                          }
                        , { user_id = 3
                          , scores = Array.fromList [ 10, 20, 30, 40, 50, 670, 30, -100 ]
                          , chip = 12
                          }
                        , { user_id = 4
                          , scores = Array.fromList [ 11, 21, 31, 41, 51, 671, 31, -110 ]
                          , chip = 13
                          }
                        ]
                    }

                testValue =
                    E.encode 0 <| Miyabq.resultsEncoder resultDto
            in
            \_ ->
                Expect.equal expectedValue testValue
        ]


log : Test
log =
    describe "Log.elm"
        [ test "toResultDto4" <|
            let
                expectedValue =
                    { match_date = "2022-01-03"
                    , results =
                        [ { user_id = 1

                          -- miya
                          -- 69
                          , scores = Array.fromList [ 86, -26, 1, 8 ]
                          , chip = 12
                          }
                        , { user_id = 2

                          -- とも
                          -- -37
                          , scores = Array.fromList [ 6, 31, -23, -51 ]
                          , chip = 27
                          }
                        , { user_id = 4

                          -- きっしー
                          -- -93
                          , scores = Array.fromList [ -23, -64, -62, 56 ]
                          , chip = -57
                          }
                        , { user_id = 5

                          -- さっしー
                          -- 61
                          , scores = Array.fromList [ -69, 59, 84, -13 ]
                          , chip = 18
                          }
                        ]
                    }

                testValue =
                    Log.toResultDto4
                        { createdAt = Time.millisToPosix 1641221202000
                        , playerIds = StaticArray.fromList Length.four 5 [ 4, 1, 2 ]
                        , rounds =
                            Array.fromList
                                [ Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "-85" [ "167", "661", "257" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "-100" [ "0", "100", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "483" [ "-38", "142", "413" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "-100", "0", "100" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "641" [ "-17", "207", "169" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "100" [ "-100", "0", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "268" [ "460", "281", "-9" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "0", "0", "0" ]
                                    }
                                ]
                        , chips = Array.fromList [ 18, -57, 12, 27 ]
                        , rankPoint = ( 10, 20 )
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "toResultDto4 ポイントが1人分しか入力されていない round が削除されること" <|
            let
                expectedValue =
                    { match_date = "2022-01-03"
                    , results =
                        [ { user_id = 1

                          -- miya
                          -- 69
                          , scores = Array.fromList [ 86, -26, 1, 8 ]
                          , chip = 12
                          }
                        , { user_id = 2

                          -- とも
                          -- -37
                          , scores = Array.fromList [ 6, 31, -23, -51 ]
                          , chip = 27
                          }
                        , { user_id = 4

                          -- きっしー
                          -- -93
                          , scores = Array.fromList [ -23, -64, -62, 56 ]
                          , chip = -57
                          }
                        , { user_id = 5

                          -- さっしー
                          -- 61
                          , scores = Array.fromList [ -69, 59, 84, -13 ]
                          , chip = 18
                          }
                        ]
                    }

                testValue =
                    Log.toResultDto4
                        { createdAt = Time.millisToPosix 1641221202000
                        , playerIds = StaticArray.fromList Length.four 5 [ 4, 1, 2 ]
                        , rounds =
                            Array.fromList
                                [ Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "-85" [ "167", "661", "257" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "-100" [ "0", "100", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "483" [ "-38", "142", "413" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "-100", "0", "100" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "641" [ "-17", "207", "169" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "100" [ "-100", "0", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "268" [ "460", "281", "-9" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "0", "0", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "268" [ "", "", "" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "0", "0", "0" ]
                                    }
                                ]
                        , chips = Array.fromList [ 18, -57, 12, 27 ]
                        , rankPoint = ( 10, 20 )
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "toResultDto4 ポイントが1人分も入力されていない round が削除されること" <|
            let
                expectedValue =
                    { match_date = "2022-01-03"
                    , results =
                        [ { user_id = 1

                          -- miya
                          -- 69
                          , scores = Array.fromList [ 86, -26, 1, 8 ]
                          , chip = 12
                          }
                        , { user_id = 2

                          -- とも
                          -- -37
                          , scores = Array.fromList [ 6, 31, -23, -51 ]
                          , chip = 27
                          }
                        , { user_id = 4

                          -- きっしー
                          -- -93
                          , scores = Array.fromList [ -23, -64, -62, 56 ]
                          , chip = -57
                          }
                        , { user_id = 5

                          -- さっしー
                          -- 61
                          , scores = Array.fromList [ -69, 59, 84, -13 ]
                          , chip = 18
                          }
                        ]
                    }

                testValue =
                    Log.toResultDto4
                        { createdAt = Time.millisToPosix 1641221202000
                        , playerIds = StaticArray.fromList Length.four 5 [ 4, 1, 2 ]
                        , rounds =
                            Array.fromList
                                [ Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "-85" [ "167", "661", "257" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "-100" [ "0", "100", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "483" [ "-38", "142", "413" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "-100", "0", "100" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "641" [ "-17", "207", "169" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "100" [ "-100", "0", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "268" [ "460", "281", "-9" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "0", "0", "0" ]
                                    }
                                , Rounds.test__round4
                                    { points =
                                        StaticArray.fromList Length.four "268" [ "", "", "" ]
                                    , seatingOrder = Nothing
                                    , tobisho = StaticArray.fromList Length.four "0" [ "0", "0", "0" ]
                                    }
                                ]
                        , chips = Array.fromList [ 18, -57, 12, 27 ]
                        , rankPoint = ( 10, 20 )
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        ]


exTime : Test
exTime =
    describe "ExTime.elm"
        [ test "posixToYmdWithHyphenDelimiter" <|
            let
                expectedValue =
                    "2022-01-03"

                testValue =
                    posixToYmdWithHyphenDelimiter (Time.millisToPosix 1641221202000)
            in
            \_ ->
                Expect.equal expectedValue testValue
        ]


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
                        { round_ =
                            Rounds.test__intRound4
                                { seatingOrder =
                                    Just
                                        { ton = 0
                                        , nan = 1
                                        , sha = 2
                                        , pei = 3
                                        }
                                , points = StaticArray.fromList Length.four 100 [ 200, 300, 400 ]
                                , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
                                }
                        , rankPoint = ( 10, 20 )
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
                        { round_ =
                            Rounds.test__intRound4
                                { seatingOrder =
                                    Just
                                        { ton = 0
                                        , nan = 2
                                        , sha = 1
                                        , pei = 3
                                        }
                                , points = StaticArray.fromList Length.four 100 [ 200, 200, 500 ]
                                , tobiSho = StaticArray.fromList Length.four 0 [ 0, 0, 0 ]
                                }
                        , rankPoint = ( 10, 20 )
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
                        , tobiSho = StaticArray.fromList Length.four -100 [ 100, 0, 0 ]
                        }

                testValue =
                    Rounds.calculateRoundFromRawPoint
                        { round_ =
                            Rounds.test__intRound4
                                { seatingOrder =
                                    Just
                                        { ton = 0
                                        , nan = 1
                                        , sha = 2
                                        , pei = 3
                                        }
                                , points = StaticArray.fromList Length.four 100 [ 200, 300, 400 ]
                                , tobiSho = StaticArray.fromList Length.four -100 [ 100, 0, 0 ]
                                }
                        , rankPoint = ( 10, 20 )
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3万点返しする" <|
            let
                expectedValue =
                    StaticArray.fromList Length.four ( 0, -200 ) [ ( 1, -100 ), ( 2, 0 ), ( 3, 100 ) ]

                testValue =
                    Rounds.returnedPoints 30 <|
                        StaticArray.fromList Length.four 100 [ 200, 300, 400 ]
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
                    Rounds.addRankPointPoints
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
                    Rounds.roundFirstDegit 3555
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "4桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    2020

                testValue =
                    Rounds.roundFirstDegit 2019
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    350

                testValue =
                    Rounds.roundFirstDegit 355
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "3桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    360

                testValue =
                    Rounds.roundFirstDegit 356
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    10

                testValue =
                    Rounds.roundFirstDegit 13
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "2桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    40

                testValue =
                    Rounds.roundFirstDegit 39
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること1" <|
            let
                expectedValue =
                    0

                testValue =
                    Rounds.roundFirstDegit 2
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること2" <|
            let
                expectedValue =
                    10

                testValue =
                    Rounds.roundFirstDegit 7
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "1桁の整数の五捨六入が正しくできること3" <|
            let
                expectedValue =
                    0

                testValue =
                    Rounds.roundFirstDegit 0
            in
            \_ ->
                Expect.equal expectedValue testValue
        ]
