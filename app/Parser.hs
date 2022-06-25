module Parser where

import Control.Applicative
import ParserCombinator
import Tools
import CFG

charP :: Char -> Parser Char
charP c = Parser $ \s ->
    case s of
        []       -> Nothing
        (x : xs) -> if (x == c) then Just (xs, x) else Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

sepCharP :: Parser String
sepCharP = Parser $ \s ->
    case runParser (predicate (\c -> ((c == ' ') || (c == '\t') || (c == '\n')))) s of
        Nothing            -> Just (s, "")
        Just (rest, token) -> Just (rest, token)

unsignedIntegerP :: Parser Integer
unsignedIntegerP = sepCharP *> (str2Int <$> (sepCharP *> (predicate isDigit) <* sepCharP)) <* sepCharP

indentifierP :: Parser Indentifier
indentifierP = sepCharP *> (Indentifier <$> (sepCharP *> idP <* sepCharP)) <* sepCharP
    where 
        idP = (\s1 s2 -> s1 ++ s2) <$> (predicate isLetter) <*> tmpP
        tmpP = Parser $ \s -> case runParser (predicate (\c -> isDigit c || isLetter c)) s of
            Nothing            -> Just (s, "")4       
            Just (rest, token) -> Just (rest, token)

exprP :: Parser Expr
exprP = sepCharP *> (Expr <$> markP <*> itemP <*> tmpP) <* sepCharP
    where
        markP = sepCharP *> (
                Parser $ \s -> case runParser (predicate (\c -> c == '+' || c == '-')) s of
                    Nothing           -> Just (s, "")
                    Just (rest, token) -> Just (rest, token) )
                <* sepCharP
        tmpP = many ((\a b -> (a, b)) <$> (charP '+' <|> charP '-') <*> itemP) <|> pure []

itemP :: Parser Item
itemP = sepCharP *> (Item <$> factorP <*> tmpP) <* sepCharP
    where
        tmpP = many ((\a b -> (a, b)) <$> (charP '*' <|> charP '/') <*> factorP) <|> pure []

factorP :: Parser Factor
factorP = sepCharP *> (Parser f) <* sepCharP
    where
        f s = case runParser indentifierP s of
                Just (rest, token)             -> Just (rest, FactorId token)
                _ -> case runParser unsignedIntegerP s of
                        Just (rest, token)     -> Just (rest, FactorInt token)
                        _                  ->  case runParser (sepCharP *> charP '(' *> exprP <* charP ')' <* sepCharP) s of
                            Just (rest, token) -> Just (rest, FactorExpr token)
                            _                  -> Nothing