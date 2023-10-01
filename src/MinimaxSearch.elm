module MinimaxSearch exposing (findBestMove, Evaluation(..), negateEvaluation)

{-| This library implements minimax algorithm with alpha-beta pruning.

@docs findBestMove, Evaluation, negateEvaluation

-}


{-| Evaluation of a game
-}
type Evaluation
    = Winning
    | Loosing
    | Score Int


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
    , possibleMoves : { isYourTurn : Bool } -> game -> List move
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
    , possibleMoves : { isYourTurn : Bool } -> game -> List move
    , evaluate : game -> Evaluation
    , apply : move -> game -> game
    }
    -> { maxValue : Evaluation, move : Maybe move }
algoMax args =
    let
        moves =
            args.possibleMoves { isYourTurn = args.isYourTurn }
                args.game
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
                            algoMax
                                { isYourTurn = not args.isYourTurn
                                , depth = args.depth - 1
                                , maxDepth = args.maxDepth
                                , game = args.apply move args.game
                                , move = Just move
                                , possibleMoves = args.possibleMoves
                                , evaluate = args.evaluate
                                , apply = args.apply
                                }
                                |> .maxValue
                    in
                    if
                        (args.isYourTurn && isBiggerThen output.maxValue value)
                            || (not args.isYourTurn && isBiggerThen value output.maxValue)
                    then
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
                { maxValue =
                    if args.isYourTurn then
                        Loosing

                    else
                        Winning
                , move = Nothing
                }


isBiggerThen : Evaluation -> Evaluation -> Bool
isBiggerThen b a =
    case ( a, b ) of
        ( Winning, Winning ) ->
            False

        ( Loosing, Loosing ) ->
            False

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


negateEvaluation : Evaluation -> Evaluation
negateEvaluation intOrInf =
    case intOrInf of
        Winning ->
            Loosing

        Loosing ->
            Winning

        Score a ->
            Score -a
