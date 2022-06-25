module CFG where

import Tools

data Indentifier = Indentifier String
    deriving (Show)

data Expr = Expr String Item [(Char, Item)]
    deriving (Show)

data Item = Item Factor [(Char, Factor)]
    deriving (Show)

data Factor = FactorId Indentifier | FactorInt Integer | FactorExpr Expr 
    deriving (Show)