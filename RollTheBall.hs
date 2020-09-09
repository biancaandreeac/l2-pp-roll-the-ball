{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

-- possible directions
data Directions = North | South | West | East | Err
    deriving (Show, Eq, Ord)

-- Represents a position
type Position = (Int, Int)

-- board cells data type
data Cell = Cell Char deriving (Eq, Ord, Show)

-- current level data type
data Level = Lvl (A.Array (Int, Int) Cell)
	deriving (Eq, Ord)

-- show instance for level
instance Show Level
    where show (Lvl lvl) = (++ [endl]) $ showHelper (Lvl lvl) $snd $A.bounds lvl

showHelper :: Level -> Position -> String
showHelper lvl (maxX, maxY) =
	[if (j == -1) then endl else getCellType $getCellAt lvl (i, j) | i <- [0..maxX], j <- [-1.. maxY]]

-- getters
getCellAt :: Level -> Position -> Cell
getCellAt (Lvl lvl) pos = lvl A.! pos

getCellType :: Cell -> Char
getCellType (Cell t) = t

getLvlArray :: Level -> (A.Array (Int, Int) Cell)
getLvlArray (Lvl lvl) = lvl


-- Create empty level
emptyLevel :: Position -> Level
emptyLevel (x, y) =
	Lvl $ A.array ((0, 0), (x, y)) cells
		where cells = [((i, j), (Cell emptySpace))| i <- [0..x], j <- [0..y]]

-- Add a new cell to a level if the given position is empty
addCell :: (Char, Position) -> Level -> Level
addCell (cell, (x, y)) (Lvl lvl)
		| x < 0 || y < 0 || x > maxX || y > maxY = Lvl lvl
				-- out of bounds => don't add
		| lvl A.! pos == (Cell emptySpace) = Lvl $lvl A.// [(pos, (Cell cell))]
				-- add only if the given position is empty
		| otherwise = Lvl lvl
		where
			(maxX, maxY) = snd (A.bounds lvl)
			pos = (x, y)


-- Create a new level using the cells given in a list
createLevel :: Position -> [(Char, Position)] -> Level
createLevel maxPos list =
	foldr addCell (emptyLevel maxPos) list
		-- add every Cell from the list over an empty level


-- Move a cell from a position in a direction, only if possible
moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) dir (Lvl lvl)
	| x < 0 || y < 0 || x > maxX || y > maxY = Lvl lvl
	| newX < 0 || newY < 0 || newX > maxX || newY > maxY = Lvl lvl
					-- out of bounds
	| elem cell startCells = Lvl lvl
					-- starting cells are fixed
	| elem cell winningCells = Lvl lvl
					-- winning cells are fixed
	| not $neigh == emptySpace = Lvl lvl
					-- not empty neigh
	| otherwise = Lvl $newLvl A.// [((x, y), (Cell emptySpace))]
					-- replace curr cell with empty cell
		where
			(Cell cell)  = lvl A.! (x, y);
			(Cell neigh) = lvl A.! (newX, newY);
			(Lvl newLvl) = addCell (cell, (newX, newY)) (Lvl lvl)
					-- place curr cell on empty position
			newX
				| dir == North = x - 1
				| dir == South = x + 1
				| otherwise    = x
			newY
				| dir == East = y + 1
				| dir == West = y - 1
				| otherwise   = y
			(maxX, maxY) = snd $A.bounds lvl


-- check if two cells can connect. dir tells us what neighbour of cell1 is cell2
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir
	| dir == East  || dir == West  = connectionEastWest   cell1 cell2 dir
	| dir == North || dir == South = connectionNorthSouth cell1 cell2 dir
	| otherwise = False

-- checks horizontal directions
connectionEastWest :: Cell -> Cell -> Directions -> Bool
connectionEastWest (Cell c1) (Cell c2) dir
	| elem c1 verticalList = False
	| elem c2 verticalList = False
	| dir == East = elem c1 leftList && elem c2 rightList
	| dir == West = elem c2 leftList && elem c1 rightList
	| otherwise = False
	where
		verticalList = [verPipe, startUp, startDown, winUp, winDown];
		leftList  = [horPipe, botLeft, topLeft, startRight, winRight];
		rightList = [horPipe, botRight, topRight, startLeft, winLeft];

-- checks vertical directions
connectionNorthSouth :: Cell -> Cell -> Directions -> Bool
connectionNorthSouth (Cell c1) (Cell c2) dir
	| elem c1 horizontalList = False
	| elem c2 horizontalList = False
	| dir == North = elem c1 downList && elem c2 upList
	| dir == South = elem c2 downList && elem c1 upList
	| otherwise = False
	where
		horizontalList = [horPipe, startRight, startLeft, winRight, winLeft];
		upList   = [verPipe, topLeft, topRight, startDown, winDown];
		downList = [verPipe, botLeft, botRight, startUp, winUp];


-- checks if the game is over
wonLevel :: Level -> Bool
wonLevel lvl = 
	isCont lvl (Cell start) pos dir
	where
		((Cell start), pos) = findStart lvl
		dir
			| start == startLeft = West
			| start == startRight = East
			| start == startUp = North
			| start == startDown = South
			| otherwise = Err

-- finds the start cell of the level and returns it with its position
findStart :: Level -> (Cell, Position)
findStart (Lvl lvl) = (getCellAt (Lvl lvl) pos, pos)
	where
		[pos] = filter checkPos positions
		checkPos p = func $ getCellAt (Lvl lvl) p
		func (Cell x) = elem x startCells
		positions = [(i, j) | i <- [0.. xMax], j <- [0.. yMax]]
		(xMax, yMax) = snd (A.bounds lvl)

-- checks if there is a continuous way to get from start to win.
isCont :: Level -> Cell -> Position -> Directions -> Bool
isCont (Lvl lvl) (Cell val) (x, y) dir
	| newX < 0 || newY < 0 || newX > xMax || newY > yMax = False
	| elem val winningCells = True
	| connection (Cell val) (Cell kind) dir = isCont (Lvl lvl) (Cell kind) (newX, newY) nextDir
	| otherwise = False
	where
		newX
			| dir == North = x - 1
			| dir == South = x + 1
			| otherwise    = x
		newY
			| dir == East = y + 1
			| dir == West = y - 1
			| otherwise   = y
		(Cell kind) = lvl A.! (newX, newY)
		nextDir
			| (kind == horPipe) || (kind == verPipe) = dir
			| kind == topLeft  = if (dir == West) then South else East
			| kind == botLeft  = if (dir == West) then North else East
			| kind == topRight = if (dir == East) then South else West
			| kind == botRight = if (dir == East) then North else West
			| otherwise = Err
		(xMax, yMax) = snd $A.bounds lvl


instance ProblemState Level (Position, Directions) where

    successors lvl =
    	foldl (++) [] $map (oneEmptySpaceSuccesor lvl) emptySpaces
    	where
    		emptySpaces = findEmptySpaces lvl

    isGoal lvl = wonLevel lvl

    reverseAction (((x, y), dir), lvl) = 
    	(((newX, newY), newDir), moveCell (newX, newY) newDir lvl)
    	where
    		newX
    			| dir == North = x - 1
    			| dir == South = x + 1
    			| otherwise    = x
    		newY
    			| dir == West = y - 1
    			| dir == East = y + 1
    			| otherwise   = y
    		newDir
    			| dir == North = South
    			| dir == South = North
    			| dir == West = East
    			| dir == East = West
    			| otherwise = Err


-- functions that help creating the successors
findEmptySpaces :: Level -> [Position]
findEmptySpaces (Lvl lvl) = filter checkPos positions
	where
		checkPos p = func $ getCellAt (Lvl lvl) p
		positions = [(i, j) | i <- [0.. xMax], j <- [0.. yMax]]
		(xMax, yMax) = snd $A.bounds lvl
		func (Cell x) = x == emptySpace

oneEmptySpaceSuccesor :: Level -> Position -> [((Position, Directions), Level)]
oneEmptySpaceSuccesor (Lvl lvl) (xEmpty, yEmpty) =
	map generatePair $validPositions (Lvl lvl) (xEmpty, yEmpty)
    	where
    		generatePair (x, y) =
    			(((x, y), (newDir x y)), (moveCell (x, y) (newDir x y) (Lvl lvl)))
    		newDir x y
    			| x < xEmpty = South
    			| x > xEmpty = North
    			| y < yEmpty = East
    			| y > yEmpty = West
    			| otherwise  = Err

-- (x, y) - position of an empty space
-- checks who can go into that empty space
validPositions :: Level -> Position -> [Position]
validPositions (Lvl lvl) (x, y) =
	filter canMove $ filter onBoard possiblePositions
		where
			possiblePositions = [(x - 1, y),  -- up
							 	 (x + 1, y),  -- down
							 	 (x, y - 1),  -- left
							 	 (x, y + 1)]  -- right
			canMove pos = notFixed pos && (notEmpty (lvl A.! pos))
			notFixed pos = (notStartingCell (lvl A.! pos)) && (notWinningCell (lvl A.! pos))
			notStartingCell (Cell cell) = not $ elem cell startCells
			notWinningCell (Cell cell) = not $ elem cell winningCells
			notEmpty (Cell cell) = not $ cell == emptySpace
			onBoard (xCur, yCur) = xCur >= 0 && xCur <= xMax && yCur >= 0 && yCur <= yMax
			(xMax, yMax) = snd $A.bounds lvl


------------------------------------ bonus helpers ------------------------------------------
heuristic :: (Num a, Ord a) => Level -> a
heuristic lvl
	| isGoal lvl = 0
	| otherwise = manhattanDistMatrix A.! (xFin, yFin)
	where
		(xW, yW) = findWin lvl
		(x, y) = lastConnectedElem lvl (Cell start) startpos dirSt
		(xFin, yFin) = if (x < 0|| y < 0) then (xW, yW) else (x, y)
		manhattanDistMatrix = createHeuristicMatrix lvl (xW, yW)
		((Cell start), startpos) = findStart lvl
		dirSt
			| start == startLeft = West
			| start == startRight = East
			| start == startUp = North
			| start == startDown = South
			| otherwise = Err

-- base function to create the distance matrix
createHeuristicMatrix :: (Num a, Ord a) => Level -> Position -> (A.Array (Int, Int) a)
createHeuristicMatrix (Lvl lvl) (xWin, yWin) =
	hMatrixHelper emptyMatrix (xWin, yWin) 1
	where
		emptyMatrix = A.array (A.bounds lvl) [((i, j), 0) | i <- [0..xMax], j <- [0..yMax]]
		(xMax, yMax) = snd $A.bounds lvl

-- helper function. recursively construct the distances from the winning position
hMatrixHelper :: (Num a, Ord a) => (A.Array (Int, Int) a) -> Position -> a -> (A.Array (Int, Int) a)
hMatrixHelper mat (x, y) step =
	modifyUp $ modifyDown $ modifyLeft $ modifyRight mat
	where
		-- functions to modify the values around the given pos, if possible
		modifyUp matr
			| isOk (x + 1, y) matr = hMatrixHelper (matr A.// [((x + 1, y), step)]) (x + 1, y) (step + 1)
			| otherwise = matr
		modifyDown matr
			| isOk (x - 1, y) matr = hMatrixHelper (matr A.// [((x - 1, y), step)]) (x - 1, y) (step + 1)
			| otherwise = matr
		modifyLeft matr
			| isOk (x, y - 1) matr = hMatrixHelper (matr A.// [((x, y - 1), step)]) (x, y - 1) (step + 1)
			| otherwise = matr
		modifyRight matr
			| isOk (x, y + 1) matr = hMatrixHelper (matr A.// [((x, y + 1), step)]) (x, y + 1) (step + 1)
			| otherwise = matr

		-- checks if a position is still on board, and if the value has to be changed
		isOk pos matrix = (validate pos) && ((matrix A.! pos > step) || (matrix A.! pos == 0))
		-- checks if it's on board
		validate (xCur, yCur) = xCur >= 0 && xCur <= xMax && yCur >= 0 && yCur <= yMax
		(xMax, yMax) = snd $A.bounds mat


-- finds the start cell of the level and returns it with its position
findWin :: Level -> Position
findWin (Lvl lvl) = head $filter checkPos positions
	where
		(xMax, yMax) = snd (A.bounds lvl)
		checkPos p = func $ getCellAt (Lvl lvl) p
		func (Cell x) = elem x winningCells
		positions = [(i, j) | i <- [0.. xMax], j <- [0.. yMax]]
		

-- finds the position of the last element that is connected with the start cell
lastConnectedElem :: Level -> Cell -> Position -> Directions -> Position
lastConnectedElem (Lvl lvl) (Cell val) (x, y) dir
	| newX < 0 || newY < 0 || newX > xMax || newY > yMax = (-1,-1)
	| connection (Cell val) (Cell kind) dir =
			lastConnectedElem (Lvl lvl) (Cell kind) (newX, newY) nextDir
	| otherwise = (x, y)
	where
		newX
			| dir == North = x - 1
			| dir == South = x + 1
			| otherwise    = x
		newY
			| dir == East = y + 1
			| dir == West = y - 1
			| otherwise   = y
		(Cell kind) = lvl A.! (newX, newY)
		nextDir
			| (kind == horPipe) || (kind == verPipe) = dir
			| kind == topLeft  = if (dir == West) then South else East
			| kind == botLeft  = if (dir == West) then North else East
			| kind == topRight = if (dir == East) then South else West
			| kind == botRight = if (dir == East) then North else West
			| otherwise = Err
		(xMax, yMax) = snd $A.bounds lvl
