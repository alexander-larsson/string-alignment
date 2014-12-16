===============================================================================================
HOME ASSIGNMENT 3, EDAN40, FUNCTIONAL PROGRAMMING
===============================================================================================
AUTHOR: Alexander Larsson, dat11ala
===============================================================================================


===============================================================================================
ANSWERS
===============================================================================================

1. Sort the two sequences that are to be compared and convert them into strings. Then run the
string align alignment algorithm with the score values set to:

scoreMatch = 1
scoreMismatch = 0
scoreSpace = 0

The algorithm will then find the maximum amount of matches which is the MCS of the two
sequences.


2.b It takes two heads of type a and a list of pairs containing lists of type a. For every pair
in the list it will attach the heads at the first position in each of the lists it contains. The
first head, h1, is attached to the first list in the tuple and h2 is attached to the second list
in the tuple. The same two heads are attached to all tuples in the list.

===============================================================================================
CODE
===============================================================================================
\begin{code}
module Alignment where
import Data.List

type AlignmentType = (String,String)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = scoreSpace * length (y:ys)
similarityScore (x:xs) [] = scoreSpace * length (x:xs)
similarityScore (x:xs) (y:ys) =  maximum [similarityScore xs ys + score(x,y), similarityScore xs (y:ys) + score(x,'-'), similarityScore (x:xs) ys + score('-',y)]

fastSimilarityScore :: String -> String -> Int
fastSimilarityScore xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
       
    simEntry :: Int -> Int -> Int
    simEntry i 0 = scoreSpace*i
    simEntry 0 j = scoreSpace*j
    simEntry i j
      | x == y    = scoreMatch + simScore (i-1) (j-1)
      | otherwise = maximum [scoreMismatch + simScore (i-1) (j-1), scoreSpace + simScore i (j-1), scoreSpace + simScore (i-1) j]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

score :: (Char, Char) -> Int
score (x,'-') = scoreSpace
score ('-',y) = scoreSpace
score (x, y)
	| x == y = scoreMatch
	| otherwise = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails t1 t2 aList = [(xs++[t1],ys++[t2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy f xs = last . groupBy (\x y -> (f x) == (f y)) $ (sortBy (\x y -> compare (f x) (f y)) xs)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [("","")]
optAlignments [] ys = [(replicate (length ys) '-',ys)]
optAlignments xs [] = [(xs,replicate (length xs) '-')]
optAlignments (x:xs) (y:ys) = maximaBy quickScore $ concat [attachHeads x y (optAlignments xs ys), attachHeads x '-' (optAlignments xs (y:ys)), attachHeads '-' y (optAlignments (x:xs) ys)]
	where quickScore (xs,ys) = sum $ zipWith (curry score) xs ys

fastOptAlignments :: String -> String -> [AlignmentType]
fastOptAlignments xs ys = getAlignments (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
    
    getScore :: Int -> Int -> Int
    getScore i j = fst $ simScore i j

    getAlignments :: Int -> Int -> [AlignmentType]
    getAlignments i j = snd $ simScore i j

    simEntry :: Int -> Int -> (Int, [AlignmentType])
    simEntry 0 0 = (0,[("","")])
    simEntry i 0 = (getScore (i-1) 0 + scoreSpace, attachTails (xs!!(i-1)) '-' (getAlignments (i-1) 0))
    simEntry 0 j = (getScore 0 (j-1) + scoreSpace, attachTails '-' (ys!!(j-1)) (getAlignments 0 (j-1)))
    simEntry i j
      | x == y    = (scoreMatch + getScore (i-1) (j-1), attachTails x y (getAlignments (i-1) (j-1)))
      | otherwise = (fst $ head optimalPossibilites, concat . map snd $ optimalPossibilites)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
         pos1 = (getScore (i-1) (j-1) + scoreMismatch, attachTails x y (getAlignments (i-1) (j-1)))
         pos2 = (getScore i (j-1) + scoreSpace, attachTails '-' y (getAlignments i (j-1)))
         pos3 = (getScore (i-1) j + scoreSpace, attachTails x '-' (getAlignments (i-1) j))
         optimalPossibilites = maximaBy fst [pos1, pos2, pos3]

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
	let optAlign = fastOptAlignments string1 string2
	let lineUp (x, y) = "\n" ++ x ++ "\n" ++ y ++ "\n"
	putStrLn ("\nThere are " ++ (show . length $ optAlign) ++ " optimal alignments:\n")
	mapM_ putStrLn $ map lineUp optAlign
\end{code}