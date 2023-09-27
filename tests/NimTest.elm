module NimTest exposing (..)

import Expect
import MinimaxSearch exposing (..)
import Test exposing (..)


options =
    { apply = moveFunc
    , evaluate = valueFunc
    , possibleMoves = possibleMovesFunc
    , searchDepth = maxDepth
    }


suite : Test
suite =
    describe "NimTest.elm"
        [ test "Nim Test - 5" <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove options
                        5
                    )
                    (Just 1)
        , test "Nim Test - 4" <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove options
                        4
                    )
                    Nothing
        , test "Nim Test - 3" <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove options 3)
                    (Just 3)
        ]


moveFunc : Int -> Int -> Int
moveFunc taken position =
    position - taken


valueFunc : Int -> Evaluation
valueFunc position =
    if position == 0 then
        Winning

    else
        Score 0


possibleMovesFunc : Int -> List Int
possibleMovesFunc position =
    case position of
        0 ->
            []

        1 ->
            [ 1 ]

        2 ->
            [ 2, 1 ]

        _ ->
            [ 3, 2, 1 ]


maxDepth : Int
maxDepth =
    6
