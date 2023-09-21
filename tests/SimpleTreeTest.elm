module SimpleTreeTest exposing (..)

import Expect
import MinimaxSearch exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "SimpleTreeTes.elm"
        [ test "MiniMax Test Depth 4." <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove
                        { apply = moveFunc
                        , evaluate = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , searchDepth = 4
                        }
                        ""
                    )
                    ('0' |> Just)
        , test "MiniMax Test Depth 2." <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove
                        { apply = moveFunc
                        , evaluate = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , searchDepth = 2
                        }
                        ""
                    )
                    ('0' |> Just)
        , test "MiniMax Test Depth 1." <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove
                        { apply = moveFunc
                        , evaluate = valueFunc
                        , possibleMoves = possibleMovesFunc
                        , searchDepth = 1
                        }
                        ""
                    )
                    ('1' |> Just)
        , test "MiniMax Test Depth 2. No possible moves" <|
            \() ->
                Expect.equal
                    (MinimaxSearch.findBestMove
                        { apply = moveFunc
                        , evaluate = valueFunc
                        , possibleMoves = possibleMovesFuncEmpty
                        , searchDepth = 2
                        }
                        ""
                    )
                    Nothing
        ]



--                  4 (5)                         max
--                 /  \
--                /    \
--               /      \
--              /        \
--             /          \
--            /            \
--           /              \
--          /                \
--         4 (3)             2 (5)                min
--        / \              / |  \
--       /   \            /  |   \
--      /     \          /   |    \
--     /       \        /    |     \
--    6         4       3    2      3             max
--  /   \     /   \     |    |    /   \
-- 5    6    4    1     3    2   3     1          min
--/ \   |    |   / \   / \  / \  |   / | \
--8 5   6    4   1 2   3 5  6 2  3   5 7 1        max


moveFunc : Char -> String -> String
moveFunc move position =
    position ++ String.fromChar move


valueFunc : String -> Evaluation
valueFunc position =
    Score <|
        case position of
            "" ->
                5

            "0" ->
                3

            "1" ->
                5

            "00" ->
                6

            "01" ->
                4

            "10" ->
                3

            "11" ->
                1

            "12" ->
                2

            "000" ->
                5

            "001" ->
                6

            "010" ->
                4

            "011" ->
                1

            "100" ->
                3

            "110" ->
                2

            "120" ->
                3

            "121" ->
                1

            "0000" ->
                8

            "0001" ->
                5

            "0010" ->
                6

            "0100" ->
                4

            "0110" ->
                1

            "0111" ->
                2

            "1000" ->
                3

            "1001" ->
                5

            "1100" ->
                6

            "1101" ->
                2

            "1200" ->
                3

            "1210" ->
                5

            "1211" ->
                7

            "1212" ->
                1

            _ ->
                0


possibleMovesFuncEmpty : String -> List Char
possibleMovesFuncEmpty _ =
    []


possibleMovesFunc : String -> List Char
possibleMovesFunc position =
    case position of
        "" ->
            [ '0', '1' ]

        "0" ->
            [ '0', '1' ]

        "00" ->
            [ '0', '1' ]

        "000" ->
            [ '0', '1' ]

        "001" ->
            [ '0' ]

        "01" ->
            [ '0', '1' ]

        "010" ->
            [ '0' ]

        "011" ->
            [ '0', '1' ]

        "1" ->
            [ '0', '1', '2' ]

        "10" ->
            [ '0' ]

        "100" ->
            [ '0', '1' ]

        "11" ->
            [ '0' ]

        "110" ->
            [ '0', '1' ]

        "12" ->
            [ '0', '1' ]

        "120" ->
            [ '0' ]

        "121" ->
            [ '0', '1', '2' ]

        _ ->
            []
