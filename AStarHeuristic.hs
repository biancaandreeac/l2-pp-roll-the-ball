{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleContexts, InstanceSigs #-}

module AStarHeuristic where
import RollTheBall
import ProblemState

import Search

import Data.Hashable
import Data.Graph.AStar
import qualified Data.HashSet as H
import Data.Array as A

import RollLevels

{-
		aStar 	:: (Hashable a, Ord a, Ord c, Num c)
			=> (a -> HashSet a)
			-> (a -> a -> c)
			-> (a -> c)
			-> (a -> Bool)
			-> a
			-> Maybe [a], a fiind tipul Level.
-}

levelToList :: Level -> [Cell]
levelToList (Lvl lvl) = A.elems lvl

-- Hashing functions
instance Hashable Cell where
	hashWithSalt :: Int -> Cell -> Int
	hashWithSalt i (Cell ch) = hashWithSalt i ch

instance Hashable Level where
	hashWithSalt :: Int -> Level -> Int
	hashWithSalt i lvl = hashWithSalt i $levelToList lvl


-- returns the root's neighbours
neighbours :: (Level -> H.HashSet Level)
neighbours lvl = H.fromList $map nodeState $nodeChildren $createStateSpace lvl

-- the distance between 2 neighbours is always 1
distance :: (Num c) => (Level -> Level -> c)
distance _ _ = 1

-- banal heuristic
trivialHeuristic :: (Num a) => Level -> a
trivialHeuristic _ = 1

-- heuristic function, uses the Manhattan Distance
nonTrivialHeuristic :: (Num c, Ord c) => Level -> c
nonTrivialHeuristic lvl = heuristic lvl		

-- if its a final node, return true
isGoalNode :: Level -> Bool
isGoalNode = isGoal
