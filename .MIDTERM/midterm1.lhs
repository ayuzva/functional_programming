\documentclass{article}
%include polycode.fmt
\begin{document}

{\Large \textbf{Comp Sci 3FP3 Midterm 1}}

\noindent
You are expected to use Haskell to do this assignment.
Code that does not compile will be worth no marks. If you
want to submit 'pseudocode', do it in comments -- you may
get partial marks then.

\noindent
You may use any function from the Haskell Prelude and any
Haskell feature you wish to answer the questions. Useful
definitions can be found at the end.

\begin{code}
module Midterm1 where

-- see at end
import Prelude hiding (maximum, minimum, length, map, zip)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent
Q1 (4 marks)

\noindent
The |zip| function
\begin{code}
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []
\end{code}
\noindent
\emph{loses information}. 
\begin{enumerate}
  \item Figure out what \emph{loses information} means from the definition
       above, and explain what it means in the context of |zip|.

     Loses information, is about the cases where one list is shorter than the other.
     In that case information no zipped form the longer list is lost.

  \item There is however an explicit mathematical invariant that holds between
       the length of the input lists and the result of |zip|. Find and
        write out this invariant.

     The invariant is the result list will always be min length of the two input lists.

\end{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Q2 (6 marks)

\noindent
Consider the following definitions:
\begin{code}
data LeftLeaning a  = LNil | LCons (LeftLeaning a) a

data RightLeaning a = RNil | RCons a (RightLeaning a)


meld :: (a -> b -> c) -> a -> b -> LeftLeaning a -> RightLeaning b -> [c]
meld f x y LNil RNil = [(f x y)]
meld f x y (LCons q z) RNil = [(f x y)] ++ [(f z y)] ++ (meld f x y q RNil) 

\end{code}
\noindent Implement |meld|.  Every argument of |meld| must be used in
at least one case of the implementation. \textbf{All the elements} in 
the inputs of type |LeftLeaning a| and |RightLeaning b|
must be used.

\noindent
For this function, like for the previous one, there is an explicit
invariant that holds between
the inputs of type |LeftLeaning a| and
|RightLeaning b| and the length of the output. What is it?

|meld| is a variation of a function that is in the Haskell
Prelude.  Which one, and explain why.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Q3 {2 marks}

\noindent
Write a function that given a list of lists returns
only the lists of 3 or more elements. Ensure you maintain the list order.
Also make sure your function works even when ``inner'' lists
are infinite, such as |[[12,13],[1,2,3],[4..],[-4..],[1],[0]]|,
where the result would be |[[1,2,3],[4..],[-4..]]|.
A simpler example: Given |[[1,2,3],[4,5],[6,7,8,9]]| 
\emph{the result should be} |[[1,2,3],[6,7,8,9]]|.

\begin{code}

funcA :: [[a]] -> [[a]]
funcA  [] = [] 
funcA (ls@(x : y : z : xs) : zs) = [take 1 ls] ++ funcA zs 



\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Q4 (6 marks)

\noindent
Prove that the following property always holds. 
\begin{code}
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)               -- swap.1

prop_midterm :: Eq a => Integer -> [a] -> Bool
prop_midterm n xs = zip [n..] xs == map swap (zip xs [n..])
\end{code}
\noindent Hint: Induction on the list.

\noindent
Does your proof mean that this property holds for infinite list too?
(Explain your answer, this is not a yes/no question).

Proof:
     zip [n..] xs == map swap (zip xs [n..])
By induction on xs:
     Base case xs = []:
          zip [n..] []
     <definition of zip.2>
          []
     <definition of map.1>
          map swap []
     <definition of zip.2>
          map swap (zip [] [n..])
     Induction step xs = x:xs: -- Induction hypothesis: zip [n..] xs == map swap (zip xs [n..])
          zip [n..] x:xs
     <definition of zip.1>
          (n,x) : zip [..] xs
     <induction hypothesis>
          (n,x) : map swap (zip xs [..])
     <definition of swap>
          swap (x,n) : map swap (zip xs [..])
     <definition of map.2>
          map swap (zip xs [n..])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Q5 (5 marks)

\noindent
Here is the definition of a |Rose| tree:
\begin{code}
data Rose a = Rose a [Rose a]
\end{code}

\noindent
Write a function to compute the \emph{depth}
of a tree. |Rose 110 []| should have depth 0,
and |Rose 5 [Rose 3 [Rose 17 []]]| should have depth 2.

\begin{code}
depth :: Rose a -> Int
depth (Rose a []) = 0
depth (Rose a [x]) = 1 + depth x
depth (Rose a (x:xs)) = 1 + maximum (map depth xs) 

\end{code}
\noindent
Write a function to compute the number of nodes (i.e. number of occurrences of
the constructor |Rose|) in your tree?
\begin{code}
number :: Rose a -> Int
number (Rose a []) = 1
number (Rose a [x]) = 1 + number x
number (Rose a (x:xs)) = 1 + foldr (+) 0 (map number xs)
\end{code}
\noindent
Write a function to compute the maximal branching, i.e. the size of the largest
list contained in the tree?
\begin{code}
maxBrch :: Rose a -> Int
maxBrch (Rose a []) = 0
maxBrch (Rose a [x]) = max (1) (maxBrch x)
maxBrch (Rose a (x:xs)) = max (1 + length xs)  (maximum (map maxBrch xs)) 
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
maximum :: Ord a => [a] -> a
maximum [] = error "maximum of empty list does not exist"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

minimum :: Ord a => [a] -> a
minimum [] = error "minimum of empty list does not exist"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

length :: [a] -> Int
length []     = 0                  -- len.1
length (_:xs) = 1 + length xs      -- len.2

map :: (a -> b) -> [a] -> [b]
map _ [] = []                      -- map.1
map f (x : xs) = f x : map f xs    -- map.2
\end{code}
\end{document}
