import Data.Char
import Data.List
import Data.Matrix

data Direction = L | R deriving (Eq, Show, Bounded, Enum, Read)

data Cardinal = North | South | East | West deriving (Eq, Show, Bounded, Enum)

type Instruction = (Char, Direction)

type Board = Matrix Char

data Ant = Ant {
	x :: Int,
	y :: Int,
	d :: Cardinal
	} deriving (Show)

data State = State {
	ant :: Ant,
	board :: Board,
	instructions :: [Instruction],
	size:: Int
	}

	
colorList = ['A'..'z'] ++ map intToDigit [0..]


listDirections::[String] -> [Direction]
listDirections = map read 

createInstructions::[Direction] -> [Instruction]
createInstructions commands = zip (take (length commands) colors) commands
	where colors = colorList

createBoard:: Int -> [Instruction] -> Board
createBoard size instructions = fromLists [[firstColor | n <- [0..(size-1)] ] | n <- [0..(size-1)] ]
	where firstColor = fst $ head instructions

antX state = x $ ant state
antY state = y $ ant state

step::State -> State
step state = state{board = deltaBoard, ant = deltaAnt}
	where 	deltaAnt = computeAntStep state
		deltaBoard = computeBoardStep state	

turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

computeAntStep::State -> Ant
computeAntStep state = currentAnt{x = newX, y=newY, d=newCaridinal}
	where 	currentAnt = ant state
		currentCardinal = d currentAnt
		colorDir = getDirection state (antX state) (antY state)
		newCaridinal = if(colorDir == L) then turnLeft currentCardinal else turnRight currentCardinal
		(newX,newY) = newAntXY state newCaridinal

newAntXY:: State -> Cardinal->(Int,Int)
newAntXY state cardinal = case cardinal of
	North -> if(antsY -1 >= 1) then (antsX,antsY-1) else (antsX, bound)
	South -> if(antsY + 1 <= bound) then (antsX, antsY+1) else (antsX, 1)
	West  -> if(antsX - 1 >=1) then (antsX-1,antsY) else (bound, antsY) 
	East  -> if(antsX + 1 <= bound) then (antsX+1,antsY) else (1,antsY)
	where 	antsX = antX state
		antsY = antY state	
		bound = size state - 1

computeBoardStep::State -> Board
computeBoardStep state = setElem newColor (antX state, antY state) (board state)
	where 	newColor = succColor ((board state) ! (antsX, antsY)) state
		antsX = antX state
		antsY = antY state	
		


getColor state x y =  fst $ getInstruction state x y
getDirection state x y=  snd $  getInstruction state x y

--Gets the instruction for the color at board x y
getInstruction state x y = instructions' !!  getIndexOfColor instructions' ((board state) ! (x,y)) 
	where instructions' = instructions state



getIndexOfColor instructions color= indexOfInfiniteList color $ justColors instructions

justColors instructions = [c | (c,_) <- instructions]

succColor ::  Char -> State -> Char
succColor currentColor state = nextColor
	where 	currentIndex = getIndexOfColor (instructions state) currentColor
		nextColor=  (cycle $ justColors $ instructions state ) !! (1+ currentIndex)

indexOfInfiniteList::(Eq a)=> a -> [a] -> Int
indexOfInfiniteList elem list = indexSeeded 0 elem list
	where 	indexSeeded currentPos elem (x:xs) = if elem == x then currentPos else indexSeeded (currentPos+1) elem xs


stepTimes 0 state = state
stepTimes n state = stepTimes (n-1) (step state)

stepInteractive n state = do 
	if (n == 0) 
		then return () 
		else do
			let newState = step state
			print $ ant state
			print $ board newState
			stepInteractive (n-1) newState
	

main = do
	print "Input Instructions: "
	instructions <- fmap (createInstructions .  listDirections .  words) $ getLine
	print "Input size: "
	size <- readLn::IO Int
	print "Steps: "	
	steps <- readLn::IO Int
	
	let state = State{board = (createBoard size instructions), instructions = instructions, size = size, ant = initialAnt size}
	print "Step Through? True,False: "
	doStep <- readLn::IO Bool
	if doStep 
		then stepInteractive steps state	
		else print $ board $ stepTimes steps state
	

	
	
	--print $ simulate board size (initialAnt size) 
	return ()

	where initialAnt size = Ant{x=size-1, y=size-1, d=North}

