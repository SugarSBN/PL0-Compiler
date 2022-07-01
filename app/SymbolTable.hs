module SymbolTable where

import CFG

data Kind = Constant | Variable | Procedure
    deriving (Show)
data Entry = Entry {
    name  :: String,
    kind  :: Kind,
    val   :: Integer,
    level :: Integer,
    addr  :: Integer
} deriving (Show)

type SymTable = [Entry]

genConstDef :: ConstDefine -> Integer -> Entry
genConstDef (ConstDefine name val) level = Entry name Constant val level 0

genConst :: ConstState -> Integer -> SymTable
genConst consts level = 
    [ genConstDef (consts !! i) level | i <- [0 .. (length consts - 1)]]

genVarDef :: Identifier -> Integer -> Integer -> Entry
genVarDef name level addr = Entry name Variable 0 level addr

genVar :: VarState -> Integer -> SymTable
genVar vars level = 
    [ genVarDef (vars !! i) level (toInteger (3 + i)) | i <- [0 .. (length vars - 1)]]

genProcedureDef :: (ProcedureHead, SubProgram) -> Integer -> Entry
genProcedureDef (ProcedureHead name, sub) level = Entry name Procedure 0 level 0 

genProcedure :: Procedures -> Integer -> SymTable
genProcedure procedures level = 
    [genProcedureDef (procedures !! i) level | i <- [0 .. (length procedures - 1)]]

genSubProgram :: SubProgram -> Integer -> SymTable
genSubProgram (SubProgram constState varState procedures command) level = 
    (genConst constState level)      ++ 
    (genVar varState level)          ++ 
    (genProcedure procedures level)  ++
    foldl (++) [] [genSubProgram (snd (procedures !! i)) (level + 1) | i <- [0 .. (length procedures - 1)]]
    

genSymTable :: (String, SubProgram) -> Maybe SymTable
genSymTable (s, p) = Just $ genSubProgram p 0