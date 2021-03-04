module Testing where

    doubleMe x = x + x

    doubleUs x y = x*2 + y*2

    doubleSmallNumber x = if x > 100
                            then x
                            else x*2

    length' :: (Num b) => [a] -> b
    length' [] = 0
    length' (_ : xs) = 1 + length' xs

    sum' :: (Num a) => [a] -> a
    sum' [] = 0
    sum' (x : xs) = x + sum' xs

    capital :: String -> String
    capital "" = "Empty String"
    capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

    myCompare :: (Ord a) => a -> a -> Ordering
    a `myCompare` b
        | a > b     = GT
        | a == b    = EQ
        | otherwise = LT

    guardTest :: (RealFloat a) => a -> String
    guardTest inVar
        | inVar <= 18.5 = "Test 1"
        | inVar <= 25.0 = "Test 2"
        | inVar <= 30.0 = "Test 3"
        | otherwise = "Test 4"

    betterGuardTest :: (RealFloat a) => a -> a -> String
    betterGuardTest weight height
        | bmi <= thresh1 = "Test 1"
        | bmi <= thresh2 = "Test 2"
        | bmi <= thresh3 = "Test 3"
        | otherwise = "Test 4"
        where bmi = weight / height ^ 2  
            thresh1 = 18.5  
            thresh2 = 25.0  
            thresh3 = 30.0  

    main :: IO ()
    main = print "Hello world!"

    