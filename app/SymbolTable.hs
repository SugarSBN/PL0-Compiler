module SymbolTable where

import CFG

data Kind = Constant | Variable | Procedure
    deriving (Show, Eq)
data Entry = Entry {
    name  :: String,
    kind  :: Kind,
    val   :: Integer,
    level :: Integer,
    addr  :: Integer
} deriving (Show, Eq)

type SymTable = [Entry]

symConstDef :: ConstDefine -> Integer -> Entry
symConstDef (ConstDefine name val) level = Entry name Constant val level 0

symConst :: ConstState -> Integer -> SymTable
symConst consts level = 
    [ symConstDef (consts !! i) level | i <- [0 .. (length consts - 1)]]

symVarDef :: Identifier -> Integer -> Integer -> Entry
symVarDef name level addr = Entry name Variable 0 level addr

symVar :: VarState -> Integer -> SymTable
symVar vars level = 
    [ symVarDef (vars !! i) level (toInteger (3 + i)) | i <- [0 .. (length vars - 1)]]

symProcedureDef :: (ProcedureHead, SubProgram) -> Integer -> Entry
symProcedureDef (ProcedureHead name, sub) level = Entry name Procedure 0 level 0 

symProcedure :: Procedures -> Integer -> SymTable
symProcedure procedures level = 
    [symProcedureDef (procedures !! i) level | i <- [0 .. (length procedures - 1)]]

symSubProgram :: SubProgram -> Integer -> SymTable
symSubProgram (SubProgram constState varState procedures command) level = 
    (symConst constState level)      ++ 
    (symVar varState level)          ++ 
    (symProcedure procedures level)  ++
    foldl (++) [] [symSubProgram (snd (procedures !! i)) (level + 1) | i <- [0 .. (length procedures - 1)]]
    

symSymTable :: (String, SubProgram) -> Maybe SymTable
symSymTable (s, p) = Just $ symSubProgram p 0



-- Lookup table --
lookUp :: SymTable -> String -> Integer -> Maybe Entry
lookUp table nam lev = case reverse table of
    []       -> Nothing
    (x : xs) -> if ((name x == nam) && (level x <= lev)) then Just x else lookUp (reverse xs) nam lev

replace :: SymTable -> Entry -> Entry -> SymTable
replace table old new =
    case table of
        []       -> []
        (x : xs) -> if (x == old) then (new : xs) else (x : replace xs old new)