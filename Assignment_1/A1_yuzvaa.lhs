Assignment 1
3FP3
McMaster University
Andriy Yuzva

\begin{code}
--Qustion 1
matches :: Eq a => a -> [a] -> [a]
matches given []       = []
matches given (first : rest) | (given == first) = given : given `matches` rest
                             | otherwise = given `matches` rest

elem' :: Eq a => a -> [a] -> Bool
elem' given [] = False
elem' given (first : rest) | (given == first) = True
                          | otherwise = elem' given rest

assignPos :: Eq a => Integer -> a -> [a] -> [Integer]
assignPos _ given [] = []
assignPos position given (current : rest) | given == current = position : assignPos (position + 1) given rest
                                        | otherwise        = assignPos (position + 1) given rest

pos :: Eq a => a -> [a] -> [Integer]
pos given [] = [ -1 ]
pos given sequence = assignPos 0 given sequence
\end{code}

Out of the three functions, the first two are best. I believe that because these functions 
are implemented within themselves and do not rely on helper functions. Pos could be implemented 
via zip filter and map, in which case helper would not be required. Alternatively, if pos requirment
was modified to also take current index as argument it would not need helper.

\begin{code}
--Qustion 2

applyAll :: [a -> b] -> [a] -> [b]
applyAll [] values = []
applyAll functions [] = []
applyAll (function : remainingFunct) values = (function `map` values) ++ (applyAll remainingFunct values)

\end{code}

\begin{code}
--Qustion 3
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
--Qustion 4
--4 is not clear what to implement in the functions
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
functions, as well as the 'Both' case function.

\begin{code}
--Qustion 5
data Ternary a = TLeaf a | TNode (Ternary a) (Ternary a) (Ternary a)

mirror :: Ternary a -> Ternary a
mirror (TLeaf a) = (TLeaf a) 
mirror (TNode child1 child2 child3) = (TNode child3 child2 child1)

flattenTernary :: Ternary a -> [a]
flattenTernary (TLeaf a) = [a] 
flattenTernary (TNode child1 child2 child3) = (flattenTernary child1) ++ (flattenTernary child2) ++ (flattenTernary child3)
\end{code}

--Qustion 6
INSERT PROOF HERE

\begin{code}
--Question7
mystery :: ((a, b) -> c) -> [a] -> [b] -> [c]
mystery _ [] _ = []
mystery _ _ [] = []
mystery f (x : xs) (y : ys) = f (x, y) : (mystery f xs ys) 

\end{code}

\begin{code}
--Question8
--foldr :: (a -> b -> b) -> b -> [a] -> b
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = foldr (\x y -> y ++ [x]) [] xs
\end{code}

\begin{code}
--Question9
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
--Prove pre (mirrorTree t) = reverse (post t)

\begin{code}
--Question10


\end{code}