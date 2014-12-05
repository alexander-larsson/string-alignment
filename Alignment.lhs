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

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = scoreSpace * length (y:ys)
similarityScore (x:xs) [] = scoreSpace * length (x:xs)
similarityScore (x:xs) (y:ys) =  maximum [similarityScore xs ys + score(x,y), similarityScore xs (y:ys) + score(x,'-'), similarityScore (x:xs) ys + score('-',y)]

score :: (Char, Char) -> Int
score (x,'-') = scoreSpace
score ('-',y) = scoreSpace
score (x, y)
	| x == y = scoreMatch
	| otherwise = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = last . groupBy (\x y -> (f x) == (f y)) $ (sortBy (\x y -> compare (f x) (f y)) xs)

\end{code}