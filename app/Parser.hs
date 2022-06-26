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

identifierP :: Parser Identifier
identifierP = sepCharP *> (Identifier <$> (sepCharP *> idP <* sepCharP)) <* sepCharP
    where 
        idP = (\s1 s2 -> s1 ++ s2) <$> (predicate isLetter) <*> tmpP
        tmpP = Parser $ \s -> case runParser (predicate (\c -> isDigit c || isLetter c)) s of
            Nothing            -> Just (s, "")  
            Just (rest, token) -> Just (rest, token)

exprP :: Parser Expr
exprP = sepCharP *> (Expr <$> markP <*> itemP <*> tmpP) <* sepCharP
    where
        markP = sepCharP *> (
                Parser $ \s -> case runParser (predicate (\c -> c == '+' || c == '-')) s of
                    Nothing            -> Just (s, "")
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
        f s = case runParser identifierP s of
                Just (rest, token)             -> Just (rest, FactorId token)
                _ -> case runParser unsignedIntegerP s of
                        Just (rest, token)     -> Just (rest, FactorInt token)
                        _                  ->  case runParser (sepCharP *> charP '(' *> exprP <* charP ')' <* sepCharP) s of
                            Just (rest, token) -> Just (rest, FactorExpr token)
                            _                  -> Nothing

assignP :: Parser Assign
assignP = sepCharP *> ((\id _ expr -> Assign id expr) <$> identifierP <*> (sepCharP *> stringP ":=" <* sepCharP) <*> exprP) <* sepCharP

relationP :: Parser String
relationP = stringP "=" <|> stringP "#" <|> stringP "<=" <|> stringP ">=" <|> stringP "<" <|> stringP ">" 

conditionP :: Parser Condition
conditionP = sepCharP *> 
            ((Condition <$> exprP <*> (sepCharP *> relationP <* sepCharP) <*> exprP) <|> 
            (\_ a -> Odd a) <$> stringP "odd" <*> exprP) 
            <* sepCharP

procedureCallP :: Parser ProcedureCall
procedureCallP = sepCharP *> ((\_ a -> ProcedureCall a) <$> stringP "call" <*> ((predicate isSpace) *> identifierP)) <* sepCharP

procedureHeadP :: Parser ProcedureHead
procedureHeadP = sepCharP *> ((\_ a _ -> ProcedureHead a) <$> stringP "procedure" <*> ((predicate isSpace) *> identifierP <* sepCharP) <*> stringP ";") <* sepCharP

readP :: Parser ComRead
readP = sepCharP *> stringP "read" *> sepCharP *> stringP "(" *> sepCharP *> 
        (ComRead <$> sepBy (sepCharP *> charP ',' <* sepCharP) identifierP) 
        <* sepCharP <* stringP ")" <* sepCharP

writeP :: Parser ComWrite
writeP = sepCharP *> stringP "write" *> sepCharP *> stringP "(" *> sepCharP *> 
        (ComWrite <$> sepBy (sepCharP *> charP ',' <* sepCharP) identifierP) 
        <* sepCharP <* stringP ")" <* sepCharP