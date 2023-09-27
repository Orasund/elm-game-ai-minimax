module MinimaxSearch exposing
    ( Evaluation(..)
    , evaluateMoves, findBestMove
    )

{-| This library implements minimax algorithm with alpha-beta pruning.

@docs minimax, Evaluation

-}

import Util


{-| Evaluation of a game
-}
type Evaluation
    = Winning
    | Loosing
    | Score Int


type alias Options game move =
    { searchDepth : Int
    , evaluate : game -> Evaluation
    , apply : move -> game -> game
    , possibleMoves : game -> List move
    }


{-| Computes the best move

  - **apply** - is a function that takes a positon and applies a move
  - **evaluate** - evaluates the game state
  - **possibleMoves** - computes all possible next moves
  - **game** - the game state
  - **searchDepth** - Defines how deep the algorithm should search. Higher values give better results but are slower.

-}
findBestMove :
    { apply : move -> game -> game
    , evaluate : game -> Evaluation
    , possibleMoves : game -> List move
    , searchDepth : Int
    }
    -> game
    -> Maybe move
findBestMove options game =
    algoMax
        { isYourTurn = True
        , depth = options.searchDepth
        , maxDepth = options.searchDepth
        , game = game
        , move = Nothing
        , possibleMoves = options.possibleMoves
        , evaluate = options.evaluate
        , apply = options.apply
        }
        |> .move


algoMax :
    { isYourTurn : Bool
    , depth : Int
    , maxDepth : Int
    , game : game
    , move : Maybe move
    , possibleMoves : game -> List move
    , evaluate : game -> Evaluation
    , apply : move -> game -> game
    }
    -> { maxValue : Evaluation, move : Maybe move }
algoMax args =
    let
        moves =
            args.possibleMoves args.game
    in
    if args.depth == 0 || List.isEmpty moves then
        { maxValue = args.evaluate args.game
        , move = args.move
        }

    else
        moves
            |> List.foldl
                (\move output ->
                    let
                        value =
                            (algoMax
                                { isYourTurn = not args.isYourTurn
                                , depth = args.depth - 1
                                , maxDepth = args.maxDepth
                                , game =
                                    args.move
                                        |> Maybe.map (\m -> args.apply m args.game)
                                        |> Maybe.withDefault args.game
                                , move = Just move
                                , possibleMoves = args.possibleMoves
                                , evaluate = args.evaluate
                                , apply = args.apply
                                }
                            ).maxValue
                                |> negate
                    in
                    if value |> isBiggerThen output.maxValue then
                        { maxValue = value
                        , move =
                            if args.depth == args.maxDepth then
                                Just move

                            else
                                output.move
                        }

                    else
                        output
                )
                { maxValue = Loosing
                , move = args.move
                }


computeEvaluation :
    { options : Options game move
    , alpha : Evaluation
    , beta : Evaluation
    , isYourTurn : Bool
    , currentDepth : Int
    , game : game
    , bestMove : Maybe move
    }
    -> Evaluation
computeEvaluation args =
    let
        moves =
            args.options.possibleMoves args.game

        evaluation =
            args.options.evaluate args.game
    in
    if
        List.isEmpty moves
            || (args.currentDepth == 0)
    then
        evaluation

    else
        moves
            |> evaluateMoves args computeEvaluation
            |> .maxValue


evaluateMoves :
    { options : Options game move
    , alpha : Evaluation
    , beta : Evaluation
    , isYourTurn : Bool
    , currentDepth : Int
    , bestMove : Maybe move
    , game : game
    }
    ->
        ({ options : Options game move
         , alpha : Evaluation
         , beta : Evaluation
         , isYourTurn : Bool
         , currentDepth : Int
         , bestMove : Maybe move
         , game : game
         }
         -> Evaluation
        )
    -> List move
    -> { maxValue : Evaluation, bestMove : Maybe move }
evaluateMoves args rec =
    Util.foldlWhileOk
        (\move { maxValue, bestMove } ->
            let
                evaluation =
                    rec
                        { options = args.options
                        , alpha = negate args.beta
                        , beta = negate maxValue
                        , isYourTurn = not args.isYourTurn
                        , currentDepth = args.currentDepth - 1
                        , game = args.options.apply move args.game
                        , bestMove = bestMove
                        }
                        |> negate
                        |> Debug.log ("eval for depth " ++ String.fromInt args.currentDepth)
            in
            if evaluation |> isBiggerThen maxValue then
                { maxValue = evaluation
                , bestMove =
                    if args.currentDepth == args.options.searchDepth then
                        Just move

                    else
                        bestMove
                }
                    |> (if
                            (maxValue == args.beta)
                                || (maxValue |> isBiggerThen args.beta)
                        then
                            Err

                        else
                            Ok
                       )

            else
                Ok
                    { maxValue = maxValue
                    , bestMove = bestMove
                    }
        )
        { maxValue = args.alpha, bestMove = args.bestMove }


isBiggerThen : Evaluation -> Evaluation -> Bool
isBiggerThen b a =
    case ( a, b ) of
        ( Winning, _ ) ->
            True

        ( Loosing, _ ) ->
            False

        ( _, Loosing ) ->
            True

        ( _, Winning ) ->
            False

        ( Score n1, Score n2 ) ->
            n1 > n2


negate : Evaluation -> Evaluation
negate intOrInf =
    case intOrInf of
        Winning ->
            Loosing

        Loosing ->
            Winning

        Score a ->
            Score -a
