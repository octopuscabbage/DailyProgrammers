import Data.Maybe
import Data.List
import Test.HUnit
import Control.Monad
import Control.Parallel.Strategies
import System.TimeIt

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine  | King | Queen | Jack | Ace deriving (Eq, Ord, Show, Bounded, Enum, Read)

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord, Show, Bounded, Enum, Read)

type Card = (Rank, Suit)
type Hand = [Card]


--I was too lazy to derive num --
rankToNum ::  Num a => Rank -> a
rankToNum Two = 2
rankToNum Three = 3
rankToNum Four = 4
rankToNum Five = 5
rankToNum Six = 6
rankToNum Seven = 7
rankToNum Eight = 8
rankToNum Nine = 9
rankToNum Ace = 1
rankToNum Jack = 10
rankToNum Queen = 11
rankToNum King = 12

hasMeld a = isJust $ findMeld $ permutations a

findMeld:: [Hand] -> Maybe Hand
findMeld [] = Nothing
findMeld (hand:hands) = if(isMeld hand) then Just hand else findMeld hands 

isMeld:: Hand -> Bool
isMeld hand = (isSet firstThree || isRun firstThree) && (isSet lastFour || isRun lastFour)
	where 	firstThree = take 3 hand
		lastFour = drop 3 hand

rankIs :: Num a =>  (a -> a -> Bool) -> Card -> Card -> Bool
rankIs f a b = f (rankToNum (fst a)) (rankToNum(fst b))

isSet:: [Card]->Bool
isSet cards = passesBinaryTest rankEqual cards
	where rankEqual a b = rankIs (==) a b

isRun:: [Card]->Bool
isRun cards = passesBinaryTest isRunBinary cards
	where 	sameSuit a b = (snd a ) == (snd b)
		isRunBinary a b = (rankIs (<) a b) && (sameSuit a b)

passesBinaryTest:: (Card -> Card -> Bool) -> Hand -> Bool
passesBinaryTest _ [] 		= True
passesBinaryTest f (a:b:xs) 	= if(f a b) then passesBinaryTest f (b:xs) else False
passesBinaryTest f (a:b:[])    	= (f a b)
passesBinaryTest f (a:[]) 	= True

replaceCard card hand = map (insert card hand) [0..7]
	where insert card hand pos = beginning  ++ [card] ++ drop 1 last
		where 	splitHand = splitAt pos hand
			beginning = fst splitHand
			last = snd splitHand
			replace a = card
readCard = do
	rankIn <- getLine
	let rank = read rankIn :: Rank
	suitIn <- getLine
	let suit = read suitIn :: Suit
	return (rank,suit)
{--main = do
	print "Hand: "
	hand <- replicateM 7 readCard
	print "Card: "
	card <- readCard
	let isWinning = map (\a -> (a,hasMeld a)) (replaceCard card hand)
	let hasWon = any (\a -> snd a == True) isWinning
	let outString = if(hasWon) then "Winning hand!" else "Impossible!"
	let winningHands = fst $ filter(\a -> snd a == True) isWinning !! 0 
	timeIt $ print outString
	print winningHands
	return ()
--}
{--
assertTrue str test = TestCase $ assertEqual str True test


testThatSetMeldWorks = assertTrue "Tests for working meld set" (hasMeld [(Jack,Spades),(Jack,Hearts),(Jack,Diamonds),(King,Spades),(King,Hearts),(King,Diamonds),(King,Clubs)])

testThatRunMeldWorks = assertTrue "Tests for working meld run" (hasMeld [(Ace,Spades),(Two,Spades),(Three,Spades),(Four,Spades),(Jack,Hearts),(Queen,Hearts),(King,Hearts)])

testThatBothWork = assertTrue "Tests for both working" (hasMeld [(Two,Diamonds),(Three,Diamonds),(Four,Diamonds),(Seven,Diamonds),(Seven,Clubs),(Seven,Hearts),(Five,Diamonds)])

testThatAllWorks = assertTrue "Tests for everything working" (any (True==) (map hasMeld (replaceCard card hand)))
	where 	card = (Five,Diamonds)
		hand = [(Two,Diamonds),(Three,Diamonds),(Four,Diamonds),(Seven,Diamonds),(Seven,Clubs),(Seven,Hearts),(Jack,Hearts)]

testReplaceCardWorks = assertTrue "Tests that card replacement is working" $ any (notElem (Jack,Hearts)) $ replaceCard (Five,Diamonds) [(Two,Diamonds),(Three,Diamonds),(Four,Diamonds),(Seven,Diamonds),(Seven,Clubs),(Seven,Hearts),(Jack,Hearts)]

--TestMain
main = runTestTT $ TestList [testThatSetMeldWorks,testThatRunMeldWorks,testThatBothWork,testThatAllWorks,testReplaceCardWorks]
--}
{--
--TimeMain
main = do
	let card = (Five,Diamonds)
	let hand = [(Two,Diamonds),(Three,Diamonds),(Four,Diamonds),(Seven,Diamonds),(Seven,Clubs),(Seven,Hearts),(Jack,Hearts)]
	let isWinning = map (\a -> (a,hasMeld a)) (replaceCard card hand)
	let hasWon = any (\a -> snd a == True) isWinning
	let outString = if(hasWon) then "Winning hand!" else "Impossible!"
	let winningHands = fst $ filter(\a -> snd a == True) isWinning !! 0 
	timeIt $ print outString
	
	let isWinningPar = parMap rseq (\a -> (a,hasMeld a)) (replaceCard card hand)
	let hasWonPar = any (\a -> snd a == True) isWinningPar
	let outStringPar = if(hasWonPar) then "Winning hand!" else "Impossible!"
	let winningHands = fst $ filter(\a -> snd a == True) isWinningPar !! 0 
	timeIt $ print outStringPar
	

	print winningHands
	return ()
--}
