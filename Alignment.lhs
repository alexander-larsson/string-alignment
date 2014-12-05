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

===============================================================================================
CODE
===============================================================================================
\begin{code}

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = scoreSpace * length (y:ys)
similarityScore (x:xs) [] = scoreSpace * length (x:xs)
similarityScore (x:xs) (y:ys) =  maximum [similarityScore xs ys + score(x,y), similarityScore xs (y:ys) + score(x,'-'), similarityScore (x:xs) ys + score('-',y)]


score (x,'-') = scoreSpace
score ('-',y) = scoreSpace
score (x, y)
	| x == y = scoreMatch
	| otherwise = scoreMismatch

\end{code}