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

data ComWrite = ComWrite [Expr]
    deriving (Show)

data ConstDefine = ConstDefine Identifier Integer
    deriving (Show)

data ConstState = ConstState [ConstDefine]
    deriving (Show)

data VarState = VarState [Identifier]
    deriving (Show)

data Command = CAssign Assign | CCall ProcedureCall | CRead ComRead | CWrite ComWrite | CSome [Command] | CIf Condition Command | CWhile Condition Command
    deriving (Show)

data Procedures = Procedures [(ProcedureHead, SubProgram)]
    deriving (Show)

data SubProgram = SubProgram ConstState VarState Procedures Command
    deriving (Show)