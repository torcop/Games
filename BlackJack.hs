module BlackJack where

import System.Random

import Test.QuickCheck
import Cards
import Wrapper  

----------------------------
-- Comments of the size function
-- First pattern match against size (Add (Card (Numeric 2) Hearts)
-- (Add (Card Jack Spades) Empty)) is
-- 1 + size hand - where hand is equal to (Add (Card Jack Spades) Empty)), which results in
-- 1 + 1 + size Empty 
-- 1 + 1 + 0 = 2
-----------------------------

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank (Numeric a) = a
valueRank x 
	| x == Ace	= 11
	| otherwise 	= 10 
	 
valueCard :: Card -> Integer 
valueCard (Card rank _) = valueRank rank

numberOfAces :: Hand -> Integer 
numberOfAces Empty = 0
numberOfAces (Add (Card rank _) hand)
	| rank == Ace 	= 1 + numberOfAces hand 
	| otherwise 	= numberOfAces hand

value :: Hand -> Integer
value Empty = 0
value (Add card hand)
	| val >	21	= val-noOfAces*10
	| otherwise	= val 
	where	noOfAces = numberOfAces (Add card hand)
		val = value' (Add card hand)

value' :: Hand -> Integer
value' Empty = 0
value' (Add card hand) = val'
	where val' = valueCard card + value' hand

gameOver :: Hand -> Bool
gameOver hand = val >= 21
	where val = value hand

winner:: Hand -> Hand -> Player
winner guest bank
	| value bank == 21	= Bank
	| value guest == 21	= Guest
	| gameOver guest	= Bank
	| gameOver bank		= Guest
	| value guest > value bank = Guest
	| otherwise 		= Bank

(<+) :: Hand -> Hand -> Hand
Empty <+  p = p
(Add c h) <+  p = Add c ( h <+ p)

fullDeck :: Hand
fullDeck = suitHand Clubs <+ suitHand Spades <+ suitHand Hearts <+ suitHand Diamonds

suitHand :: Suit ->Hand
suitHand s = suitHand' rs s Empty

suitHand' :: [Rank]->Suit -> Hand -> Hand
suitHand' [] s hand = hand
suitHand' (r:rs) s hand = suitHand' rs s (Add (Card r s) hand)
rs = [Ace,King,Queen,Jack,Numeric 10 ,Numeric 9,Numeric 8,Numeric 7,Numeric 6,
		Numeric 5,Numeric 4,Numeric 3,Numeric 2]

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add c deck) hand =  (deck,(Add c hand)) 

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand = if value bankHand < 16 && not(gameOver bankHand)
	       			then playBank' deck' bankHand'
			  else bankHand'
				      
	                 where (deck', bankHand') = draw deck bankHand	

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2)<+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size(h1 <+ h2)

----------------RANDOM CARD AND HAND------------------

shuffle :: StdGen -> Hand -> Hand
shuffle gen Empty = Empty
shuffle gen h = 
	    	    let (val,newGen) = randomR(1,size h) gen
		    	(newHand,card) = remove h val
 		    	restHand = shuffle newGen newHand 
		     in (Add card restHand)	
	     	    
remove ::Hand -> Integer -> (Hand,Card)
remove (Add c Empty) _ = (Empty,c)
remove (Add c h) 1 = (h,c)
remove (Add c h) n = let (finalhand,card)= remove h (n-1)
       	      	     in ((Add c finalhand),card)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h


belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen->Hand->Bool
prop_size_shuffle g h = size h == size (shuffle g h) 

---------------------------------------------------

implementation = Interface
	{iEmpty = empty
	, iFullDeck = fullDeck
	, iValue = value
	, iGameOver = gameOver
	, iWinner = winner
	, iDraw = draw
	, iPlayBank = playBank
	, iShuffle = shuffle
	}

main :: IO ()
main = runGame implementation