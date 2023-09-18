module MinMaxSearch exposing
    ( Node, minimax
    , NodeType(..)
    , IntOrInf(..)
    )

{-| This library implements minimax algorithm with alpha-beta pruning.


# Basic stuff

@docs Node, minimax


# Second stuff

@docs NodeType, IntegerExt

-}


{-| Represents a node type.
-}
type NodeType
    = Min
    | Max


{-| It extends standard Number about positive/negative infinity.
-}
type IntOrInf
    = Pos_Inf
    | Neg_Inf
    | Number Int


type alias MinimaxParams position move =
    { maxDepth : Int
    , valueFunc : Node position move -> Int
    , moveFunc : move -> position -> position
    , possibleMovesFunc : Node position move -> List move
    }


{-| Represents a node in the search tree.

  - **nodeType** -- MIN or MAX, it alternates throughout search tree levels (root is MAX, nodes at second level are MIN, nodes at third level are MAX, etc.)
  - **position** -- it tells what a solving problem's state is represented by this Node
  - **move** -- last move (an egde to nearest parent's node, or to best "move" node for root)
  - **value** -- value of node
  - **alpha** -- alpha
  - **beta** -- beta
  - **depth** -- depth of node (root node has 1)

-}
type alias Node position move =
    -- node state
    { nodeType : NodeType
    , position : position
    , move : Maybe move
    , value : IntOrInf
    , alpha : IntOrInf
    , beta : IntOrInf
    , depth : Int
    }


{-| Run minimax and returns Node represents "best move" over the search tree.

  - **apply** - is a function that takes a positon and applies a move.
  - **eval** - returns value of node.
  - **possibleMoves** - returns all possible edges from given node.
  - **start** - defines a root of search tree.
  - **maxDepth** - tells how deep the algorithm can dive into the search tree (i.e. max height of tree).

-}
minimax :
    { apply : move -> position -> position
    , eval : Node position move -> Int
    , possibleMoves : Node position move -> List move
    , start : position
    , searchDepth : Int
    }
    -> Node position move
minimax args =
    minimax_ (minimaxParams_ args.apply args.eval args.possibleMoves args.searchDepth)
        -- initial state - root of minimax tree
        { nodeType = Max
        , position = args.start
        , move = Nothing
        , value = Neg_Inf
        , alpha = Neg_Inf
        , beta = Pos_Inf
        , depth = 0
        }


minimaxParams_ : (move -> position -> position) -> (Node position move -> Int) -> (Node position move -> List move) -> Int -> MinimaxParams position move
minimaxParams_ moveFunc valueFunc possibleMovesFunc maxDepth =
    { maxDepth = maxDepth
    , valueFunc = valueFunc
    , moveFunc = moveFunc
    , possibleMovesFunc = possibleMovesFunc
    }


minimax_ : MinimaxParams p m -> Node p m -> Node p m
minimax_ minimaxParams node =
    if node.depth == minimaxParams.maxDepth then
        -- end recursion, we don't want to dive further, compute the node value by position that is holded
        leafValue minimaxParams node

    else
        case node.nodeType of
            -- node with highest value wins
            Max ->
                nodeValue minimaxParams node sortDescending

            -- node with lowest value wins
            Min ->
                nodeValue minimaxParams node sortAscending


leafValue : MinimaxParams p m -> Node p m -> Node p m
leafValue minimaxParams node =
    let
        value =
            Number (minimaxParams.valueFunc node)
    in
    -- setup it to node and also as alpha/beta constraints
    { node | value = value, alpha = value, beta = value }


nodeValue : MinimaxParams p m -> Node p m -> (Node p m -> Node p m -> Order) -> Node p m
nodeValue minimaxParams node sortFunc =
    let
        firstNode_ =
            descendants minimaxParams node (minimaxParams.possibleMovesFunc node)
                |> List.sortWith sortFunc
                |> List.head
    in
    case firstNode_ of
        Nothing ->
            node

        Just firstNode ->
            { node
                | value = firstNode.value
                , alpha = firstNode.alpha
                , beta = firstNode.beta
                , move =
                    if firstNode.depth == 1 then
                        firstNode.move

                    else
                        node.move

                -- transfer move to up only for first child, we looking for a move between root and first child
            }


descendants : MinimaxParams p m -> Node p m -> List m -> List (Node p m)
descendants minimaxParams node moves =
    if not (less node.alpha node.beta) then
        -- alpha/beta prunning
        []

    else
        case moves of
            -- no moves => no descendants
            [] ->
                []

            -- processing descendants
            move :: restMoves ->
                -- calling merge is recursive step to wide (by next sibling)
                -- calling minimax_ is recursive step into the depth
                merge minimaxParams
                    node
                    (minimax_ minimaxParams
                        { node
                            | nodeType = swapType node.nodeType -- the node type alternates
                            , position = minimaxParams.moveFunc move node.position -- compute new position (old position + move)
                            , move = Just move -- remember move to parent
                            , value =
                                if node.nodeType == Max then
                                    Pos_Inf

                                else
                                    Neg_Inf

                            -- init value for Max descendant is Min thus +Inf
                            , alpha = node.alpha -- alpha is inherited
                            , beta = node.beta -- beta is inherited
                            , depth = node.depth + 1 -- step into the depth
                        }
                    )
                    restMoves


merge : MinimaxParams p m -> Node p m -> Node p m -> List m -> List (Node p m)
merge minimaxParams parent previousSibling moves =
    let
        alpha =
            if parent.nodeType == Max then
                max parent.alpha previousSibling.alpha

            else
                parent.alpha

        beta =
            if parent.nodeType == Min then
                min parent.beta previousSibling.beta

            else
                parent.beta
    in
    previousSibling :: descendants minimaxParams { parent | alpha = alpha, beta = beta } moves



-- HELPER FUNCTIONS


swapType : NodeType -> NodeType
swapType t =
    case t of
        Min ->
            Max

        Max ->
            Min


sortDescending : Node p m -> Node p m -> Order
sortDescending node1 node2 =
    sortAscending node2 node1


sortAscending : Node p m -> Node p m -> Order
sortAscending node1 node2 =
    if less node1.value node2.value then
        LT

    else if equals node1.value node2.value then
        EQ

    else
        GT


max : IntOrInf -> IntOrInf -> IntOrInf
max a b =
    if great a b then
        a

    else
        b


min : IntOrInf -> IntOrInf -> IntOrInf
min a b =
    if less a b then
        a

    else
        b


less : IntOrInf -> IntOrInf -> Bool
less a b =
    not (great a b) && not (equals a b)


equals : IntOrInf -> IntOrInf -> Bool
equals a b =
    a == b


great : IntOrInf -> IntOrInf -> Bool
great a b =
    case a of
        Pos_Inf ->
            b /= Pos_Inf

        Neg_Inf ->
            False

        Number x ->
            case b of
                Neg_Inf ->
                    True

                Number y ->
                    x > y

                Pos_Inf ->
                    False
