module NimTest exposing (..)

import Expect
import MinMaxSearch exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "NimTest.elm"
        [ test "Nim Test - 5" <|
            \() ->
                Expect.equal
                    (MinMaxSearch.minimax
                        { apply = moveFunc
                        , eval = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , start = 5
                        , searchDepth = maxDepth
                        }
                    )
                    (Just 1)
        , test "Nim Test - 4" <|
            \() ->
                Expect.equal
                    (MinMaxSearch.minimax
                        { apply = moveFunc
                        , eval = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , start = 4
                        , searchDepth = maxDepth
                        }
                    )
                    (Just 3)
        , test "Nim Test - 3" <|
            \() ->
                Expect.equal
                    (MinMaxSearch.minimax
                        { apply = moveFunc
                        , eval = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , start = 3
                        , searchDepth = maxDepth
                        }
                    )
                    (Just 2)
        ]


moveFunc : Int -> Int -> Int
moveFunc taken position =
    position - taken


valueFunc : Int -> IntOrInf
valueFunc position =
    if position == 0 then
        MinMaxSearch.minusInfinity

    else
        Number 0


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
