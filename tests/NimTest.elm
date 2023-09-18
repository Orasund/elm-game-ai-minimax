module NimTest exposing (..)

import Expect
import MinMaxSearch exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "NimTest.elm"
        [ test "Nim Test" <|
            \() ->
                Expect.equal
                    (nodeEquals
                        (MinMaxSearch.minimax
                            { apply = moveFunc
                            , eval = valueFunc
                            , possibleMoves = possibleMovesFunc
                            , start = startPosition
                            , searchDepth = maxDepth
                            }
                        )
                        (Just 1)
                    )
                    True
        ]


nodeEquals : Node Int Int -> Maybe Int -> Bool
nodeEquals node move =
    node.move == move


moveFunc : Node Int Int -> Int -> Int
moveFunc node taken =
    node.position - taken


valueFunc : Node Int Int -> Int
valueFunc node =
    if node.position == 0 then
        if node.depth |> modBy 2 |> (==) 0 then
            10

        else
            0

    else
        5


possibleMovesFunc : Node Int Int -> List Int
possibleMovesFunc node =
    case node.position of
        0 ->
            []

        1 ->
            [ 1 ]

        2 ->
            [ 1, 2 ]

        _ ->
            [ 1, 2, 3 ]


startPosition : Int
startPosition =
    5


maxDepth : Int
maxDepth =
    6
