module BasicTest exposing (..)

import Expect
import MinimaxSearch exposing (Evaluation(..), evaluateMoves)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BasicTest.elm"
        [ test "Trivial Win" <|
            \() ->
                MinimaxSearch.evaluateMoves
                    { options =
                        { apply = \value _ -> value
                        , evaluate =
                            \position ->
                                if position == 1 then
                                    Winning

                                else
                                    Score 0
                        , possibleMoves = \_ -> List.range 0 2
                        , searchDepth = 2
                        }
                    , alpha = Loosing
                    , beta = Winning
                    , currentDepth = 0
                    , isYourTurn = True
                    , game = 0
                    }
                    (\_ -> Winning)
                    [ 0, 1, 2 ]
                    |> Expect.equal { bestMove = Just 1, maxValue = Winning }
        ]
