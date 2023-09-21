module BasicTest exposing (..)

import Expect
import MinMaxSearch exposing (IntOrInf(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BasicTest.elm"
        [ test "Trivial Win" <|
            \() ->
                MinMaxSearch.minimax
                    { apply = \value _ -> value
                    , eval =
                        \position ->
                            if position == 1 then
                                MinMaxSearch.infinity

                            else
                                Number 0
                    , possibleMoves = \_ -> List.range 0 2
                    , start = 0
                    , searchDepth = 2
                    }
                    |> Expect.equal (Just 1)
        ]
