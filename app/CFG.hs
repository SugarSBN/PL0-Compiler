module CFG where

import Tools

data Identifier = Identifier String
    deriving (Show)

data Expr = Expr String Item [(Char, Item)]
    deriving (Show)

data Item = Item Factor [(Char, Factor)]
    deriving (Show)

data Factor = FactorId Identifier | FactorInt Integer | FactorExpr Expr 
    deriving (Show)

data Assign = Assign Identifier Expr
    deriving (Show)

data Condition = Condition Expr String Expr | Odd Expr
    deriving (Show)

data ProcedureCall = ProcedureCall Identifier
    deriving (Show)

data ProcedureHead = ProcedureHead Identifier
    deriving (Show)

data ComRead = ComRead [Identifier]
    deriving (Show)

data ComWrite = ComWrite [Identifier]
    deriving (Show)