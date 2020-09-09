{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
import Data.List
import Data.Set as S


data Node s a =
    Node
        { state    :: s
        , action   :: Maybe a
        , parent   :: Maybe (Node s a)
        , depth    :: Int
        , children :: [Node s a]}
    deriving (Eq, Ord)


-- Node getters
nodeState :: Node s a -> s
nodeState (Node curState _ _ _ _)  = curState

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ curParent _ _) = curParent

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ curDepth _) = curDepth

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ curAction _ _ _) = curAction

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ curChildren) = curChildren

instance (Show s) => Show  (Node s a)
    where
        show = show . nodeState


--  create the entire state space tree 
createStateSpace :: (ProblemState s a, Eq s, Ord s) => s -> Node s a
createStateSpace s =
    Node s Nothing Nothing 0 curChildren
    where
        curChildren = (getChildren (successors s) (Node s Nothing Nothing 0 []) (S.singleton s))
            -- every parent that I give will have no children to avoid creating an infinite loop

-- generate node's successors
getChildren :: (ProblemState s a, Eq s, Ord s)
            => [(a, s)]   -- list of action and state of the successors
            -> Node s a   -- parent node (node without children)
            -> Set s      -- set to avoid adding two identical children
            -> [Node s a] -- children
getChildren actionState curParent set =
    Prelude.map createNode $ Prelude.filter newState actionState
    where
        curDepth = (nodeDepth curParent) + 1
        newState (_, st) = not (S.member st set)
        createNode (act, st) =
            (Node st (Just act) (Just curParent) curDepth $kids st act)
        kids st act
            | isGoal st = [] -- if the game is over then stop
            | otherwise =
                getChildren (successors st) (Node st (Just act) (Just curParent) curDepth []) (S.insert st set)


-- does a bfs on a tree
bfs :: (Ord s, Ord a)
    => Node s a   -- source node
    -> [([Node s a], [Node s a])] --[([nodes added to queue in current step], [queue])]
bfs node = recursiveBFS [node] (S.singleton (nodeState node))

recursiveBFS :: (Ord s, Ord a)
             => [Node s a]                  -- queue
             -> Set s                       -- visited
             -> [([Node s a], [Node s a])]  -- ([noduri ad pas curent], [new queue])
recursiveBFS [] _ = []
recursiveBFS (headOfQ : restQ) visited = 
    (listCur, newQ) : (recursiveBFS newQ (S.insert (nodeState headOfQ) newVisited))
    where
        listCur = Prelude.filter notDiscovered (nodeChildren headOfQ)
        newQ    = restQ ++ listCur
        notDiscovered node = notMember (nodeState node) visited
        newVisited = Data.List.foldl' (flip S.insert) visited (Data.List.map nodeState listCur)

-- does a bfs from start and one from the end until it finds a common node
-- returns the node from the fist bfs and from the second one
bidirBFS :: (Eq a, Eq s, Ord s, Ord a) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS nodeA nodeB = 
    findElems step
    where
        [step] = take 1 $ Prelude.filter findCommonState bigL
        bigL = zip la lb
        la = bfs nodeA
        lb = bfs nodeB

-- find the states that are common in both bfss
findCommonState :: (Ord s, Ord a, Eq a, Eq s)
                => (([(Node s a)],[Node s a]), ([Node s a],[Node s a]))
                -> Bool
findCommonState ((lA, _), (_, qB)) = -- list from A and queue from B
    Prelude.not (Prelude.null (Prelude.filter (isInQ qB) lA))
    where
        getStates q = Prelude.map nodeState q
        isInQ q x = elem (nodeState x) $getStates q
            -- find elems from lA that have common state as elems from Qb

-- after the elems are found we only know where they are, not who they are
findElems :: (Ord s, Ord a, Eq a, Eq s)
          => (([(Node s a)],[Node s a]), ([Node s a],[Node s a]))
          -> ((Node s a), (Node s a))
findElems  ((lA, _), (_, lB)) =
    (elemA, elemB)
    where
        (elemA:_) = Prelude.filter (isInQ lB) lA -- get first elem
        (elemB:_) = Prelude.filter (getElem) lB  -- get second elem
        getElem x = ((nodeState elemA) == (nodeState x))
        getStates q = Prelude.map nodeState q
        isInQ q x = elem (nodeState x) (getStates q)


-- first generate a list with all the parents, then create a list with 
extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath node =
    Prelude.foldr concFunc [] $ getParentList (Just node)
    where
        concFunc nod = (++ [((nodeAction nod), (nodeState nod))])

-- return a list with all the parents until root
getParentList :: Maybe (Node s a) -> [(Node s a)]
getParentList node = Prelude.map fromJust $take nr parent_list
    where
        (Just nr) = Data.List.findIndex isNothing parent_list -- find # of parents
        parent_list = iterate getParent node                  -- get parents
        getParent x = if (isNothing x) then Nothing else (nodeParent (fromJust x))


-- solve a level
solve :: (ProblemState s a, Ord s, Ord a)
      => s          -- initial state
      -> s          -- final state
      -> [(Maybe a, s)]   -- pair list

solve state1 state2 =
    path1 ++ revPath
    where
        node1 = createStateSpace state1
        node2 = createStateSpace state2
        (common1, common2) = bidirBFS node1 node2
        path1 = extractPath common1
        path2 = extractPath common2
        revPath = Prelude.reverse (reversePath (tail path2))


reversePath :: (ProblemState s a, Ord s, Ord a) => [(Maybe a, s)] -> [(Maybe a, s)]
reversePath path = Prelude.map revPath path
    where
        revPath (Nothing, s) = (Nothing, s)
        revPath ((Just a), s) = ((Just ac), st)
            where
                (ac, st) = reverseAction (a, s)
