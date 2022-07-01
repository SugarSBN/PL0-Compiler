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
    case runParser (predicate (\c -> ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r')))) s of
        Nothing            -> Just (s, "")
        Just (rest, token) -> Just (rest, token)

unsignedIntegerP :: Parser Integer
unsignedIntegerP = sepCharP *> (str2Int <$> (sepCharP *> (predicate isDigit) <* sepCharP)) <* sepCharP

identifierP :: Parser Identifier
identifierP = sepCharP *> (sepCharP *> idP <* sepCharP) <* sepCharP
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
relationP = stringP "=" <|> stringP "<>" <|> stringP "<=" <|> stringP ">=" <|> stringP "<" <|> stringP ">" 

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
        (ComWrite <$> sepBy (sepCharP *> charP ',' <* sepCharP) exprP) 
        <* sepCharP <* stringP ")" <* sepCharP

constDefineP :: Parser ConstDefine
constDefineP = sepCharP *> ((\a _ b -> ConstDefine a b) <$> identifierP <*> (sepCharP *> charP '=' <* sepCharP) <*> unsignedIntegerP ) <* sepCharP

constStateP :: Parser ConstState
constStateP = sepCharP *> (stringP "const" *> sepCharP *> sepBy (sepCharP *> charP ',' <* sepCharP) constDefineP) <* sepCharP <* charP ';' <* sepCharP

varStateP :: Parser VarState
varStateP = sepCharP *> (stringP "var" *> sepCharP *> sepBy (sepCharP *> charP ',' <* sepCharP) identifierP) <* sepCharP <* charP ';' <* sepCharP

commandP :: Parser Command
commandP = (CAssign <$> assignP) <|> (CCall <$> procedureCallP) <|> (CRead <$> readP) <|> (CWrite <$> writeP) <|> (CSome <$> someP)
       <|> ifP <|> whileP
    where
        someP = sepCharP *> stringP "begin" *> sepCharP *> (sepBy (sepCharP *> charP ';' <* sepCharP) commandP) <* sepCharP <* stringP "end" <* sepCharP
        ifP = sepCharP *> stringP "if" *> ((\a _ b -> CIf a b) <$> conditionP <*> stringP "then" <*> commandP) <* sepCharP
        whileP = sepCharP *> stringP "while" *> ((\a _ b -> CWhile a b) <$> conditionP <*> stringP "do" <*> commandP) <* sepCharP

proceduresP :: Parser Procedures
proceduresP = tmpP 
    where
        tmpP = many ((sepCharP *> ((\a _ b -> (a, b)) <$> procedureHeadP <*> sepCharP <*> subProgramP) <* sepCharP <* charP ';' <* sepCharP))

subProgramP :: Parser SubProgram
subProgramP = SubProgram <$> guardConstStateP <*> guardVarStateP <*> proceduresP <*> commandP
    where 
        guardConstStateP = Parser $ \s -> case runParser constStateP s of
            Nothing            -> Just (s, [])
            Just (rest, token) -> Just (rest, token)
        guardVarStateP = Parser $ \s -> case runParser varStateP s of
            Nothing            -> Just (s, [])
            Just (rest, token) -> Just (rest, token)