module CodeGen where

import CFG
import SymbolTable
import Tools

data Opt = LIT | LOD | STO | CAL | INT | JMP | JPC | OPR | RED | WRT
    deriving (Show)

data Assembly = Assembly {
    fieldOpt  :: Opt,
    fieldL    :: Integer,
    fieldA    :: Integer,
    callEntry :: String
}   
instance Show Assembly where
    show (Assembly opt l a ent) = (show opt) ++ " " ++ (show l) ++ " " ++ (show a) ++ " " ++ ent

genFactor :: Factor -> Integer -> SymTable -> Maybe [Assembly]
genFactor (FactorId id) lev symTable = case ent of
    Nothing -> Nothing
    Just e  -> case kind e of 
                    Variable -> Just [Assembly LOD (lev - (level e)) (addr e) ""]
                    Constant -> Just [Assembly LIT 0 (val e) ""]
    where
        ent = lookUp symTable id lev
genFactor (FactorInt x) lev symTable = Just [Assembly LIT 0 x ""]
genFactor (FactorExpr x) lev symTable = genExpr x lev symTable

genItem :: Item -> Integer -> SymTable -> Maybe [Assembly]
genItem (Item factor rest) lev symTable = case reverse rest of
    []       -> genFactor factor lev symTable
    (x : vs) -> (++) <$> (
                    (++) <$>
                    genItem (Item factor (reverse vs)) lev symTable <*>
                    genFactor (snd x) lev symTable)
                <*> case fst x of
                        '*' -> Just [Assembly OPR 0 4 ""]
                        '/' -> Just [Assembly OPR 0 5 ""]

genExpr :: Expr -> Integer -> SymTable -> Maybe [Assembly]
genExpr (Expr mark item rest) lev symTable = case reverse rest of
    []       -> (++) <$> genItem item lev symTable <*> Just (if (isNeg mark) then [Assembly OPR 0 1 ""] else [])
    (x : vs) -> (++) <$> (
                    (++) <$>
                    genExpr (Expr mark item (reverse vs)) lev symTable <*>
                    genItem (snd x) lev symTable)
                <*> case fst x of
                        '+' -> Just [Assembly OPR 0 2 ""]
                        '-' -> Just [Assembly OPR 0 3 ""]
                         
genAssign :: Assign -> Integer -> SymTable -> Maybe [Assembly]
genAssign (Assign id expr) lev symTable = case ent of
        Nothing -> Nothing
        Just e  -> (++) <$> genExpr expr lev symTable <*> Just [Assembly STO (lev - (level e)) (addr e) ""]
    where
        ent = lookUp symTable id lev

genCondition :: Condition -> Integer -> SymTable -> Maybe [Assembly]
genCondition (Odd expr) lev symTable = (++) <$> genExpr expr lev symTable <*> Just [Assembly OPR 0 6 ""]
genCondition (Condition e1 opt e2) lev symTable = 
    (++) <$> (
    (++) <$> genExpr e1 lev symTable <*> genExpr e2 lev symTable) <*>
    case opt of
        "="  -> Just [Assembly OPR 0 8 ""]
        "<>" -> Just [Assembly OPR 0 9 ""]
        "<"  -> Just [Assembly OPR 0 10 ""]
        ">=" -> Just [Assembly OPR 0 11 ""]
        ">"  -> Just [Assembly OPR 0 12 ""]
        "<=" -> Just [Assembly OPR 0 13 ""]

genProcedureCall :: ProcedureCall -> Integer -> SymTable -> Maybe [Assembly]
genProcedureCall (ProcedureCall f) lev symTable = Just [Assembly CAL 0 0 f]

genComRead :: ComRead -> Integer -> SymTable -> Maybe [Assembly]
genComRead (ComRead ids) lev symTable = case ids of
    []          -> Just []
    (id : vs) -> case lookUp symTable id lev of
        Nothing -> Nothing
        Just e  -> (++) <$> Just [Assembly RED (lev - (level e)) (addr e) ""] <*> genComRead (ComRead vs) lev symTable

genComWrite :: ComWrite -> Integer -> SymTable -> Maybe [Assembly]
genComWrite (ComWrite exprs) lev symTable = case exprs of
    []          -> Just []
    (expr : vs) -> (++) <$> ((++) <$> genExpr expr lev symTable <*> Just [Assembly WRT 0 0 ""]) <*> genComWrite (ComWrite vs) lev symTable


genSubProgram :: SubProgram -> Integer -> Maybe [Assembly]
genSubProgram (SubProgram constState varState procedures command) lev = Just $ 
    [Assembly INT 0 (toInteger (length varState)) ""] ++
    [Assembly OPR 0 0 ""]


