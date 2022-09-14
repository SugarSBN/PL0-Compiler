module Parser where

import qualified Text.Parsec as P
import Text.Parsec.String 
import Text.Parsec.Char
import Control.Applicative
import Control.Monad
import Data.Char
import CFG

-- | White spaces
whiteSpace :: Parser ()
whiteSpace = do _ <- many $ oneOf [' ', '\n', '\t', '\r']
                return ()

-- | Integers
integer :: Parser Integer
integer = do n <- P.many1 digit
             return (read n)
          P.<?> "integers"

-- | Identifiers
identifier :: Parser Identifier
identifier = do whiteSpace
                fc   <- firstChar
                rest <- many nonFirstChar
                whiteSpace
                return (fc : rest)
             P.<?> "identifiers"
             where
                  firstChar = satisfy (\a -> isLetter a || a == '_')
                  nonFirstChar = satisfy (\a -> isLetter a || isDigit a || a == '_')

-- | parens
parens :: Parser a -> Parser a
parens p = do whiteSpace
              _ <- char '('
              whiteSpace
              b <- p
              whiteSpace
              _ <- char ')'
              whiteSpace
              return b 

-- | Expressions
expression :: Parser Expr
expression = do sym <- many $ oneOf [' ', '+', '-']
                head <- item
                whiteSpace
                rest <- many (do whiteSpace
                                 opt <- char '+' <|> char '-'
                                 whiteSpace
                                 itm <- item
                                 return (opt, itm))
                whiteSpace
                let n = length $ filter (== '-') sym
                return $ Expr (if odd n then "-" else "+") head rest
                  

-- | Items
item :: Parser Item
item = do whiteSpace
          head <- factor
          rest <- many (do whiteSpace
                           opt <- char '*' <|> char '/'
                           whiteSpace
                           fac <- factor
                           return (opt, fac))
          whiteSpace
          return $ Item head rest

-- | Factors
factor :: Parser Factor
factor = do whiteSpace
            fac <- factorInt <|> factorVar <|> factorExpr
            whiteSpace
            return fac
         where
          factorInt = do int <- integer
                         return $ FactorInt int
          factorVar = do id <- identifier
                         return $ FactorId id
          factorExpr = do expr <- parens expression
                          return $ FactorExpr expr

----------------------------------------------------------------------------------

-- | Assign
assign :: Parser Assign
assign = do whiteSpace 
            id <- identifier
            whiteSpace
            _ <- string ":="
            whiteSpace
            expr <- expression
            return $ Assign id expr 

-- | Condition
condition :: Parser Condition
condition = P.try (do whiteSpace
                      exp1 <- expression
                      whiteSpace
                      rop <- relation
                      whiteSpace
                      exp2 <- expression
                      whiteSpace
                      return $ Condition exp1 rop exp2)
            <|> 
            do whiteSpace
               _ <- string "odd"
               _ <- char ' '
               whiteSpace
               exp <- expression
               whiteSpace
               return $ Odd exp
            where
                 relation = string "=" <|> P.try (string "<>") <|> P.try (string "<=") <|> P.try (string ">=") <|> string "<" <|> string ">"
                            P.<?> "relation operator"

-- | Procedure call
procedureCall :: Parser ProcedureCall
procedureCall = do whiteSpace
                   _ <- string "call"
                   _ <- char ' '
                   whiteSpace
                   id <- identifier
                   whiteSpace
                   return $ ProcedureCall id

-- | Procedure head
procedureHead :: Parser ProcedureHead
procedureHead = do whiteSpace
                   _ <- string "procedure"
                   _ <- char ' '
                   whiteSpace
                   id <- identifier
                   whiteSpace
                   _ <- char ';'
                   whiteSpace
                   return $ ProcedureHead id

-- | Command read
comRead :: Parser ComRead
comRead = do whiteSpace
             _ <- string "read"
             whiteSpace
             ids <- parens (P.sepBy identifier (char ','))
             whiteSpace
             return $ ComRead ids

-- | Command write
comWrite :: Parser ComWrite
comWrite = do whiteSpace
              _ <- string "write"
              whiteSpace
              exps <- parens (P.sepBy expression (char ','))
              whiteSpace
              return $ ComWrite exps

-- | Const definition
constDefine :: Parser ConstDefine
constDefine = do whiteSpace
                 id <- identifier
                 whiteSpace
                 _ <- char '='
                 whiteSpace
                 int <- integer
                 whiteSpace
                 return $ ConstDefine id int

-- | Const state
constState :: Parser ConstState
constState = do whiteSpace
                _ <- string "const"
                _ <- char ' '
                whiteSpace
                cons <- P.sepBy constDefine (char ',')
                whiteSpace
                _ <- char ';'
                whiteSpace
                return $ cons

-- | Varable state
varState :: Parser VarState
varState = do whiteSpace
              _ <- string "var"
              _ <- char ' '
              whiteSpace
              ids <- P.sepBy identifier (char ',')
              whiteSpace
              _ <- char ';'
              whiteSpace
              return $ ids

-- | Commands
command :: Parser Command
command = P.try (CAssign <$> assign)        <|>
          P.try (CCall   <$> procedureCall) <|>
          P.try (CRead   <$> comRead)       <|>
          P.try (CWrite  <$> comWrite)      <|>
          P.try (CSome   <$> some)          <|>
          P.try cif                         <|>
          P.try cwhile
          where
               some = do whiteSpace
                         _ <- string "begin"
                         whiteSpace
                         coms <- P.sepBy command (char ';')
                         whiteSpace
                         _ <- string "end"
                         whiteSpace
                         return coms
               cif = do whiteSpace
                        _ <- string "if"
                        whiteSpace
                        con <- condition
                        whiteSpace
                        _ <- string "then"
                        whiteSpace
                        com <- command
                        whiteSpace
                        return $ CIf con com
               cwhile = do whiteSpace
                           _ <- string "while"
                           con <- condition
                           whiteSpace
                           _ <- string "do"
                           com <- command
                           whiteSpace
                           return $ CWhile con com

-- | Procedures
procedures :: Parser Procedures
procedures = many pro
             where
                  pro = do whiteSpace
                           h <- procedureHead
                           whiteSpace
                           p <- subProgram
                           whiteSpace
                           _ <- char ';'
                           whiteSpace
                           return $ (h, p)

-- | SubPrograms
subProgram :: Parser SubProgram
subProgram = do whiteSpace
                cons <- guardCons
                whiteSpace
                vars <- guardVars
                whiteSpace
                pros <- procedures
                whiteSpace
                coms <- command
                whiteSpace
                return $ SubProgram cons vars pros coms
             where
                  guardCons = do con <- P.optionMaybe constState
                                 return $ case con of
                                             Nothing -> []
                                             Just t  -> t
                  guardVars = do var <- P.optionMaybe varState
                                 return $ case var of
                                             Nothing -> []
                                             Just t  -> t

               