\begin{code}
--DATA is similar to newtype, but used
--when constructors has more than one field inside it

--TYPE  is a just allias for existing data type type Identifier = String

--NEWTYPE is similar to data, but only allows constuctors with one
--field in it

--CLASS is a TYPE CLASS!!!!, instance belong to the type class
-- a way to group similar types

--INSTANCE is instance of a type class

module Mar04 where

type Parse a b = [a] -> [(b, [a])]

data Parsed a
    = Failure
    | Success {value :: a, remainingInput :: String}
    deriving (Show, Eq, Ord)

-- instance Show (Parser a) where my attempt at Haskell
--    show = show "HI"

newtype Parser a = MkP {applyParser :: String -> Parsed a}
--    deriving (Show)

--Functor applies a function to contents of the box to get output contents of the box
--Applicaitve applies a function in a box to contents in the box to get output contents in a box

instance Functor Parsed where
    fmap _ Failure = Failure
    fmap f (Success v rest) = Success (f v) rest
 
instance Functor Parser where
    fmap f p@(MkP z) = MkP $ (fmap f . applyParser p)


{- runParsers :: Praser a -> String -> a
runParser p inp = case ((applyParser p) inp) of
    Failure -> error "Parser failed!"
    Success a _ -> a -}

testFunct:: String -> String 
testFunct x = do
            z <- x
            zs <- x
            show [z] ++ ['b'] ++ [zs]

\end{code}
That for Parser then "maps inside". This is tircky as the instance of
(-> b) of Functor!

Binding operator in monads could be thought of as "AND THEN", chaining calcs

