module Tests exposing (..)

import Expect
import Pages.EditLog.Rounds as Rounds
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


rounds : Test
rounds =
    describe "A Test Suite"
        [ test "Round4 の順位点を計算する" <|
            let
                expectedValue =
                    Rounds.intRound4ForTesting1Expected

                testValue =
                    Rounds.calculateRoundFromRawPoint
                        { round = Rounds.intRound4ForTesting1
                        , rankPoint = ( 20, 10 )
                        , havePoint = 25
                        , returnPoint = 30
                        }
            in
            \_ ->
                Expect.equal expectedValue testValue
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
