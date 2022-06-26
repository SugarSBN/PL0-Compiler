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

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

isLetter :: Char -> Bool
isLetter 'a' = True
isLetter 'b' = True
isLetter 'c' = True
isLetter 'd' = True
isLetter 'e' = True
isLetter 'f' = True
isLetter 'g' = True
isLetter 'h' = True
isLetter 'i' = True
isLetter 'j' = True
isLetter 'k' = True
isLetter 'l' = True
isLetter 'm' = True
isLetter 'n' = True
isLetter 'o' = True
isLetter 'p' = True
isLetter 'q' = True
isLetter 'r' = True
isLetter 's' = True
isLetter 't' = True
isLetter 'u' = True
isLetter 'v' = True
isLetter 'w' = True
isLetter 'x' = True
isLetter 'y' = True
isLetter 'z' = True
isLetter 'A' = True
isLetter 'B' = True
isLetter 'C' = True
isLetter 'D' = True
isLetter 'E' = True
isLetter 'F' = True
isLetter 'G' = True
isLetter 'H' = True
isLetter 'I' = True
isLetter 'J' = True
isLetter 'K' = True
isLetter 'L' = True
isLetter 'M' = True
isLetter 'N' = True
isLetter 'O' = True
isLetter 'P' = True
isLetter 'Q' = True
isLetter 'R' = True
isLetter 'S' = True
isLetter 'T' = True
isLetter 'U' = True
isLetter 'V' = True
isLetter 'W' = True
isLetter 'X' = True
isLetter 'Y' = True
isLetter 'Z' = True
isLetter _   = False

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _ = False