Assignment 1
3FP3
McMaster University
Andriy Yuzva
yuzvaa

\begin{code}
--{-# OPTIONS_GHC -Wall #-}
module Assignment_1 where
\end{code}

\begin{code}
-------------------------------Qustion 1-------------------------------
matches :: Eq a => a -> [a] -> [a]
matches _ []       = []
matches given (first : rest) | (given == first) = given : given `matches` rest
                             | otherwise = given `matches` rest
                             
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' given (first : rest) | (given == first) = True
                          | otherwise = elem' given rest

assignPos :: Eq a => Integer -> a -> [a] -> [Integer]
assignPos _ _ [] = []
assignPos position given (current : rest) | given == current = position : assignPos (position + 1) given rest
                                        | otherwise        = assignPos (position + 1) given rest

pos :: Eq a => a -> [a] -> [Integer]
pos _ [] = [ -1 ]
pos given x = assignPos 0 given x
\end{code}

Out of the three functions, the first two are best. I believe that because these functions 
are implemented within themselves and do not rely on helper functions. Pos could be implemented 
via zip filter and map, in which case helper would not be required. Alternatively, if pos requirment
was modified to also take current index as argument it would not need helper.

\begin{code}
-------------------------------Qustion 2-------------------------------

applyAll :: [a -> b] -> [a] -> [b]
applyAll [] _ = []
applyAll _ [] = []
applyAll (function : remainingFunct) values = (function `map` values) ++ (applyAll remainingFunct values)

\end{code}

\begin{code}
-------------------------------Qustion 3-------------------------------
--Using explicit recursion
tripleNeg1 :: (Ord a, Num a) => [a] -> [a]
tripleNeg1 [] = []
tripleNeg1 (value : rest) | value >= 0  = value : tripleNeg1 rest
                          | otherwise = 3 * value : tripleNeg1 rest

tripleNeg2Decorator :: (Ord a, Num a) => [a] -> [Either a a]
tripleNeg2Decorator [] = []
tripleNeg2Decorator values = map (\x -> if (x >= 0) then Right(x) else Left(x)) values

--Using Haskell either and map
tripleNeg2 :: (Ord a, Num a) => [a] -> [a]
tripleNeg2 [] = []
tripleNeg2 values = (either (3*) (1*)) `map` tripleNeg2Decorator(values)

\end{code}

\begin{code}
-------------------------------Qustion 4-------------------------------
data OrBoth a b  = Left' a | Right' b | Both a b

consume1 :: (a -> c) -> (b -> c) -> (a -> b -> c) -> OrBoth a b -> c
consume1 f _ _ (Left' x) = f x
consume1 _ f _ (Right' x) = f x
consume1 _ _ f (Both x y) = f x y

consume2 :: (a -> c) -> (b -> c) -> (c -> c -> c) -> OrBoth a b -> c
consume2 f _ _ (Left' x) = f x
consume2 _ f _ (Right' x) = f x
consume2 fl fr f (Both x y) = f (fl x) (fr y)

\end{code}

I believe consume1 is better. It has 3 separate functions applied only once.
This scenario occurs more frequently in practice than having to apply both 
functions, as well as the 'Both' case function on top.

\begin{code}
-------------------------------Qustion 5-------------------------------
data Ternary a = TLeaf a | TNode (Ternary a) (Ternary a) (Ternary a)

mirror :: Ternary a -> Ternary a
mirror (TLeaf a) = (TLeaf a) 
mirror (TNode child1 child2 child3) = (TNode child3 child2 child1)

flattenTernary :: Ternary a -> [a]
flattenTernary (TLeaf a) = [a] 
flattenTernary (TNode child1 child2 child3) = (flattenTernary child1) ++ (flattenTernary child2) ++ (flattenTernary child3)
\end{code}

\begin{code}
-------------------------------Qustion 6-------------------------------
--Given:
all' :: (a -> Bool) -> [a] -> Bool 
all' _ [] = True 
all' p (x : xs) = p x && (all' p xs)
\end{code}
Prove:
    ∀p, xs, ys . all p (xs ++ ys) = all p xs ∧ all p ys

Theorem A : (x:xs) ++ ys = x:(xs ++ ys)

For all xs, ys:
    By induction on `xs`:
        Base case: xs = []
            all p ([] ++ ys) = all p [] ∧ all p ys
                <Definition of all, Identity of `and`>
            all p ([] ++ ys) = all p ys
                <Identity of `++`>
            all p (ys) = all p ys
                <Reflexivity of equality>
            True
        Induction step: xs = k:ks
            all p (k:ks ++ ys) = all p k:ks ∧ all p ys
                <Definition of all>
            all p (k:ks ++ ys) = (p k ∧ (all p ks)) ∧ all p ys
                <Theorem A>
            all p k:(ks ++ ys) = (p k ∧ (all p ks)) ∧ all p ys
                <Definition of all>
            p k ∧ (all p (ks ++ ys)) = (p k ∧ (all p ks)) ∧ all p ys
                <Induction hypothesis>
            p k ∧ (all p ks ∧ all p ys) = (p k ∧ (all p ks)) ∧ all p ys
                <Associativity of ∧,  Reflexivity of equality>
            True 

\begin{code}
-------------------------------Question7-------------------------------
mystery :: ((a, b) -> c) -> [a] -> [b] -> [c]
mystery _ [] _ = []
mystery _ _ [] = []
mystery f (x : xs) (y : ys) = f (x, y) : (mystery f xs ys) 

\end{code}

\begin{code}
-------------------------------Question8-------------------------------
--foldr :: (a -> b -> b) -> b -> [a] -> b
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldr (\x y -> y ++ [x]) [] xs
\end{code}

Bonus Proof:

Original reverse is defined as (per Haskell Documentation):
reverse :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

Universal propery is defined as (per "A tutorial on the universality and expressiveness of fold"):
    g [ ] = v             
    g (x : xs) = f x (g xs)  <=>  g = fold f v

To calculate parameters of the required fold we must solve the following equation:
    
    reverse l = fold f v l
        <Definition of reverse>
    rev l [] = fold f v l
        <Definition of flip>
    flip rev [] l = fold f v l
        <Same parameter for both functions,
        thus droping it to equate functions themselves>
    flip rev [] = fold f v

Therefore a flip modified rev definition would look like this:
    flip rev a     [] = a
    flip rev a (x:xs) = rev xs (x:a)

We can use universal property to get the following:
    flip rev a     [] = v
    flip rev a (x:xs) = g x (rev xs a)

From here we can see that a = v = []
We take second pattern of `flip rev`:

    flip rev a (x:xs) = g x (rev xs a)
        <Definition of flip>
    rev x:xs a = g x (rev xs a)
        <Prepending element before reversing is same as appending it after reversing>
    (rev xs a) ++ [x]= g x (rev xs a)
        <Generalize (rev xs  a) to xs>
    xs ++ [x]= g x xs 
        <Rename Variables to match to lambda function>
    y ++ [x] = g x y
        <Functions>
    g = (\x y -> y ++ [x])
    
Therefore, this is the exact lambda function that was used in my implementation.

\begin{code}
-------------------------------Question9-------------------------------
data Tree a = Tip | Node ( Tree a )  a  ( Tree a )

mirrorTree :: Tree a -> Tree a
mirrorTree  Tip = Tip
mirrorTree  (Node l a r) = Node (mirrorTree  r)  a  (mirrorTree  l)

pre :: Tree a -> [a]
pre Tip = []
pre (Node l a r) = a : ((pre l) ++ (pre r))

post :: Tree a -> [a]
post Tip = []
post (Node l a r) = ((post l) ++ (post r)) ++ [a]


\end{code}
Prove:
    pre (mirrorTree t) = reverse (post t)

Theorem A: reverse (a ++ b ++ c) = (reverse c) ++ (reverse b) ++ (reverse a)

By induction:
    Base case: t = Tip
        pre (mirrorTree Tip) = reverse (post Tip)
            <Definition of mirrorTree, Definition of post>
        pre (Tip) = reverse ([])
            <Definition of pre, Definition reverse>
        [] = []
            <Reflexivity of equality>
        True
    Induction step: t = Node x y z
        pre (mirrorTree (Node x y z) = reverse (post (Node x y z))
            <Definition of post>
        LHS = reverse ((post x) ++ (post z) ++ [y])
            <Theorem A>
        LHS = reverse [y] ++ reverse (post z) ++ reverse (post x)
            <Reverse of singleton, Induction hypothesis>
        LHS = [y] ++ pre (mirrorTree z) ++ pre (mirrorTree x)
            <Definition of pre>
        LHS = pre (Node (mirrorTree z) y (mirrorTree x))
            <Definition of mirrorTree>
        pre (mirrorTree (Node x y z) = pre (mirrorTree (Node x y z)
            <Reflexivity of equality>
        true

\begin{code}
-------------------------------Question10-------------------------------
data Rose a = Rose a [Rose a]
    deriving Show

data Fork a = Leaf a | Branch (Fork a) (Fork a)
    deriving Show
to' :: Tree a -> [Rose a]
to' Tip = []
to' (Node l n r) = [Rose n (to' l ++ to' r)]

from' :: [Rose a] -> Tree a
from' [] = Tip
from' ((Rose o i) : xs) = Node (from' i) o (from' xs)

to :: Rose a -> Fork a
to (Rose o []) = Leaf o
to (Rose o [x])  = Branch (to x) (Leaf o)
to (Rose o [x,y]) = Branch (to x) (Branch (to y) (Leaf o))
--Assuming Fork will be binary, hence Rose can't have more than two children

from :: Fork a -> Rose a
from (Leaf x)     = Rose x []
from (Branch x y) = rosePack ((\(node, roses) -> (node, ([from x] ++ roses))) (roseUnpack (from y)))

roseUnpack :: Rose a -> (a, [Rose a])
roseUnpack (Rose x ys) = (x, ys)

rosePack :: (a, [Rose a]) -> Rose a
rosePack (x, ys) = (Rose x ys)

\end{code}

Bonus Proof:
    ∀xs.to (from xs) = xs

For all xs:
    By induction on `xs`:
        Base case: xs = Leaf k
            to (from Leaf k) = Leaf k
                <Definition of from>
            to(Rose k []) = Leaf k
                <Definition of to>
            Leaf k  = Leaf k
                <Reflexivity of equality>
            True
        Induction step: xs = Branch (Fork k) (Fork m)   --Induction hypothesis = to (from xs) = xs
            to (from Branch (Fork k) (Fork m)) = Branch (Fork k) (Fork m)
                <Definition for from>
            to (rosePack ((\(node, roses) -> (node, ([from Fork k] ++ roses))) (roseUnpack (from Fork m)))) = RHS
                <Combining definition of rosePack with lambda function>
            to ((\(node, roses) -> Rose (node) ([from Fork k] ++ roses)) (roseUnpack (from Fork m))) = RHS
                <Combining definition of roseUnpack with lambda function>
            to ((\Rose node roses -> Rose (node) ([from Fork k] ++ roses)) (from Fork m)) = RHS
                <??? I am not sure how to proceed from this step. Specifically I am not sure how to
                convert `from Fork k` and `from Fork m`. Doing nested structural induction did not 
                yield the solution. >
            True