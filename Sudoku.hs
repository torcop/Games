module Sudoku where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Maybe

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )
type Block = [Maybe Int]
type Pos = (Int,Int)

-------------------------------------------------------------------------
----A

--A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 [Nothing | x <- [1..9]])

--A2
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) = case rowcol s of
	 	      True -> and (map (\a -> and [x `elem` [1..9] | (Just x) <- (filtNothing a)]) s)
		      _	      -> False

rowcol :: [[Maybe Int]] -> Bool
rowcol s = if length s == 9 then and [length x == 9 | x <-s] else False
	 	      
filtNothing :: [Maybe Int] -> [Maybe Int]
filtNothing a = [x | x <- a, (/=Nothing) x]

--A3
isSolved :: Sudoku -> Bool
isSolved (Sudoku []) =	False
isSolved (Sudoku s)  =  and $ map (/=Nothing) $concat s 

-------------------------------------------------------------------------
----B

--B1
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s) = putStrLn(unlines(map conv (map (\a -> [if x == Nothing
                             
                          then "." else val x | x <- a]) s)))
			 
conv :: [String] -> String
conv [] = ""
conv (a:as) = a++conv as

val :: Maybe Int -> String
val (Just a) = show a

--B2
readSudoku :: FilePath -> IO Sudoku
readSudoku s = do 
                 content <- readFile s
                 let sud = (Sudoku [explode x | x <- lines content])
                 if (isSudoku sud) then return sud else error "bad sudoku!"

explode :: String -> [Maybe Int]
explode "" = []
explode (x:xs) 
	       | x == '.' = Nothing:explode(xs)
	       | otherwise = (Just (digitToInt x)):explode(xs)

-------------------------------------------------------------------------
----C

--C1
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing),(1, do
                        int <- choose(1,9) 
			return (Just int))
		 ]

--C2
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

--C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku s) = isSudoku (Sudoku s)

-------------------------------------------------------------------------
----D

--D1
isOkayBlock :: Block -> Bool
isOkayBlock d = (sumInt d) + (sumEmp d) == 9
	    where 
	    	  sumInt,sumEmp :: [Maybe Int] -> Int
	    	  sumInt d = length $ nub $ filter (/= Nothing) d 
	    	  sumEmp d = length $ filter (== Nothing) d

--D2
blocks :: Sudoku -> [Block]
blocks (Sudoku s) = block s 

----helper function of blocks--------
block [] = []
block s =  row' (take 3 s) ++ block (drop 3 s)


row' ([]:_)=[]
row'  (x:y:z:[])= (take 3 x++take 3 y ++take 3 z) 
                 :row' (drop 3 x :drop 3 y:drop 3 z:[]) 

prop_block :: [Block] -> Bool
prop_block  bs = sumblock bs  == 81

prop_blocks :: Sudoku -> Bool
prop_blocks s = and [(length x) == 9 | x  <- (blocks s)] && (length (blocks s) == 9)

sumblock :: [Block]->Int
sumblock [] = 0
sumblock (b:bs)=length b + sumblock bs

--D3
isOkay::Sudoku -> Bool
isOkay (Sudoku bs) = and [isOkayBlock b |b<-bs] && and[isOkayBlock x|x<-(blocks
                           (Sudoku bs))] && and [isOkayBlock i|i<-(allCol bs 0)]

                      where allCol xs  9  = []
                            allCol xs ac = [ x !!ac|x<-xs]:allCol xs (ac+1)  

-------------------------------------------------------------------------
----E

--E1
blanks ::Sudoku ->[Pos]
blanks (Sudoku xs) = con (temp 0 xs)                     
                        where 
                          temp _    [] =[]
                          temp row (x:xs)= pos 0 row x:temp (row+1) xs 
                          pos _ _ []=[]
                          pos ac i (l:ll) |l==Nothing =(i,ac):pos (ac+1) i ll
                                          |otherwise = pos (ac+1) i ll
                                         
                          con [] = []
                          con (t:tt) = t++ con tt         
             
prop_blanks :: Sudoku -> Bool
prop_blanks s = length (blanks allBlankSudoku) == 81 && (isSolved s) == (length (blanks s) == 0)                                                       

--E2
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (i,v)=[v]
xs !!= (i,v)= take (i) xs ++[v]++(drop (i+1) xs)

prop_e2 :: Eq a => [a] -> (Int, a) -> Bool
prop_e2 a (b,c) = ((a !!= (b,c)) !! (if b <= length a && b >= 0 then b else length a)) == c

--E3
update::Sudoku ->Pos ->Maybe Int -> Sudoku
update (Sudoku sud) (row,col) item
	| (row < 0) = (Sudoku sud)
	| (col < 0) = (Sudoku sud) 
	| item == Nothing || ((fromJust item) > 0) =Sudoku (take row sud++
                                     [(sud !! row)!!=(col,item)] 
                                       ++drop (row +1)sud )


prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update s (row,col) a = (concat (rows (update s (row,col) a))) !! (row+col*9) == a


--E4
candidates::Sudoku->Pos->[Int]
candidates (Sudoku s) (row,col) =[i|i<-[1..9],i`notElem`(blockVal++rowVal++colVal)]

                                   where find' xs =[con x|x<-xs,x/=Nothing]
                                         blockVal=find' (position s(row,col))
                                         rowVal = find' (s !!row)
                                         colVal  = find'(takeColm s col)
                                                                  
                                         con ::Maybe Int ->Int    
                                         con (Just m) =  m 
                                                                                
                                         position:: [[Maybe Int]]->Pos
                                                     ->[Maybe Int]
                                         position s (row,col)=conv(row,col)
                                                               (block(temp 0 s))
                                                              
                                         temp _    [] =[]
                                         temp n (x:xs)= pos 0 n x:
                                                          temp (n+1) xs
                                         pos _ _ []=[]
                                         pos ac r (l:ll)=(r,ac,l):pos(ac+1)r ll 
                                                                 
                                         conv _ [] = error "Invalid position"
                                         conv p (t:tt)
                                             |toupleTolist p t=[c|(_,_,c)<-t]
                                             |otherwise = conv p tt  
                                         
                                         toupleTolist p poval = p`elem`
                                                        [(a,b)|(a,b,_)<-poval]  
                                                     
                                          
                                         takeColm [] _ = []
                                         takeColm (j:js) c = j!!c:takeColm js c 
                                                                                                         
    
				    
-----------------------------------------------
----F

--F1
solve ::Sudoku -> Maybe Sudoku
solve s |isSudoku s && isOkay s = solve' s 
        |otherwise = Nothing
              where 
                   solve'::Sudoku ->Maybe Sudoku

                   solve' sud |isSolved sud = Just sud
                              |otherwise = fill sud (blanks sud)

                
                   fill k [] |isSolved k && isOkay k = Just k
                             |otherwise = Nothing
                   fill k (x:xs) = branch k (candidates k x ) xs x
                   branch s' [] _  _ = Nothing
                   branch s' (c:cs) ll p = let updated = fill (update s' p (Just c)) ll   in case updated of 
                                                                                              Nothing -> branch s' cs ll p
                                                                                              _ -> updated

--F2
readAndSolve ::FilePath -> IO ()
readAndSolve path = do 
                      sud <- readSudoku path

                      let sol = solve sud in case sol of 
                                             Nothing -> putStrLn("(No solution)")
                                             _ ->printSudoku (fromJust sol)

--F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
t `isSolutionOf` s = and [(rs !! x) == (rt !! x) && (isSolved t) || (rs !! x) == Nothing | x <- [0..80]]
	where rs = concat (rows s)
	      rt = concat (rows t)

--F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = (solve s) /= Nothing ==> (fromJust (solve s)) `isSolutionOf` s

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]


