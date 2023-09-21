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
    options.possibleMoves game
        |> evaluateMoves
            { options = options
            , alpha = Loosing
            , beta = Winning
            , isYourTurn = True
            , currentDepth = 0
            , game = game
            }
            computeEvaluation
        |> .bestMove


computeEvaluation :
    { options : Options game move
    , alpha : Evaluation
    , beta : Evaluation
    , isYourTurn : Bool
    , currentDepth : Int
    , game : game
    }
    -> Evaluation
computeEvaluation args =
    let
        moves =
            args.options.possibleMoves args.game
    in
    if
        List.isEmpty moves
            || (args.currentDepth >= args.options.searchDepth)
    then
        args.options.evaluate args.game

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
    , game : game
    }
    ->
        ({ options : Options game move
         , alpha : Evaluation
         , beta : Evaluation
         , isYourTurn : Bool
         , currentDepth : Int
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
                opponentsEvaluation =
                    rec
                        { options = args.options
                        , alpha = negate args.beta
                        , beta = negate maxValue
                        , isYourTurn = not args.isYourTurn
                        , currentDepth = args.currentDepth + 1
                        , game = args.options.apply move args.game
                        }
                        |> negate
                        |> Debug.log ("eval for depth " ++ String.fromInt args.currentDepth)
            in
            if opponentsEvaluation |> isBiggerThen maxValue then
                { maxValue = opponentsEvaluation
                , bestMove =
                    if args.currentDepth == 0 then
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
        { maxValue = args.alpha, bestMove = Nothing }


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
