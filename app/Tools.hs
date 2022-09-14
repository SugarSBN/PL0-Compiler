module Tools where

str2Int :: String -> Integer
str2Int s = case reverse s of
    ""       -> 0
    (x : xs) -> (str2Int (reverse xs)) * 10 + (f x)
    where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False

isNeg :: String -> Bool
isNeg [] = False
isNeg (x : xs) = if (x == '+') then isNeg xs else not (isNeg xs)