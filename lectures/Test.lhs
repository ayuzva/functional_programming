\begin{code}

{-# OPTIONS_GHC -Wall #-}
module Test where

remSpace :: String -> String
remSpace [] = []
remSpace (x : xs) | x == ' '  = remSpace xs
                  | otherwise =  x : remSpace xs

repSpace :: String -> String
repSpace [] = []
repSpace (x : xs) | x == ' '  = '*' : (repSpace xs)
                  | otherwise = x : (repSpace xs)

--foldr (a (b c))
--foldl ((a b) c)
minFind :: Ord a => [a] -> a
minFind [] = error "No elements"
minFind (x : xs) = foldl (\z y -> if z < y then z else y) x xs

--smallP :: Orde a => [a] -> [a] -> [a]
--smallP 

interlieve :: [a] -> [a] -> [a]
interlieve [] [] = []
interlieve xs [] = xs
interlieve [] ys = ys
interlieve (x : xs) ys = x : interlieve ys xs

--map :: (a -> b) -> [a] -> [b]
--flip :: (a -> b -> c) -> b -> a -> c
--flip map :: [a] -> (a -> b) -> [b]

--[x | x <- inputList, (\z -> (z `mod` 2) == 0) x]

--data Ternary a = TLeaf a | TNode (Ternary a) (Ternary a) (Ternary a)
type TreeRoot = Node

data Edge a = Edge [Node a]
    deriving Show

data Node a = Node a [Edge a]
    deriving Show

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y) 

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p) 

--curry arg split
--uncurry arg unsplit
--uncurry curry f (a, b) = f (a, b)
--uncurry (curry f) (a, b)
--(curry f) a b


\end{code}

forall l. sum (doubleAll l) = 2 * sum l

Proof:
    Proving for all l:
        sum (doubleAll l) = 2 * sum l
    Base case `l = []`:
            sum (doubleAll [])
        =<Definition of doubleAll>
            sum (map ' (2 * ) [])
        =<Definition of map'>
            sum ([])
        =<Definition of sum>
            0
        =<Multiplication by zero>
            2*0
        =<Definition of sum>
            2*sum []
    Induction step `l = x : xs`: -- Induction hypothesis "sum (doubleAll xs) = 2 * sum xs"
            sum (doubleAll (x : xs))
        =<Definition of doubleAll>
            sum (map ' (2 * ) (x : xs))
        =<Definition of map'>
            sum ((2 * ) x : map' (2 * ) xs)
        =<Definition of doubleAll, Definition of sum, Ind Hype>
            (2 * ) x + 2 * sum xs
        =<Distributivity of multiplication, Definition of sum'>
            2 * sum (x : xs)

\begin{code}
data Rose a = Rose a [Rose a]
    deriving Show

--data Rose' String = Rose' String [Rose' String]
--    deriving Show

data Fork a = Leaf a | Branch (Fork a) (Fork a)
    deriving Show

--to' :: Tree a -> List (Rose a)


--to :: Rose a -> Fork a
--to Rose a x : xs = 


funcA :: (Eq a, Num a, Num b) => a -> b
funcA 0 = -1
funcA x | x == 2 = 2
        | x == big = secretValue
        | otherwise = -2
        where secretValue = 777
              big = 3


mystery :: ((a, b) -> c) -> [a] -> [b] -> [c]
mystery f [] _  = []
mystery f _ [] = []
mystery f (x : xs) (y : ys) = f (x, y) : (mystery f xs ys) 
\end{code}

data RE =
    Eps
  | Ch Char
  | RE :|: RE
  | RE :*: RE 
  | St RE
  | Plus RE
  deriving (Eq, Show)

\begin{code}
data Tree a = LeafT a | NodeT (Tree a) a (Tree a)
    deriving Show

mirrorTree :: Tree a -> Tree a
mirrorTree (LeafT a) = LeafT a
mirrorTree (NodeT l a r) = NodeT (mirrorTree r) a (mirrorTree l)


split :: String -> [(String, String)]
split (x : xs) =  [(x : a, b) | (a, b) <- split xs ]


\end{code}