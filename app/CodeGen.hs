module CodeGen where

import CFG
import SymbolTable
import Tools

data Opt = LIT | LOD | STO | CAL | INT | JMP | JPC | OPR | RED | WRT
    deriving (Show, Eq)

data Assembly = Assembly {
    fieldOpt  :: Opt,
    fieldL    :: Integer,
    fieldA    :: Integer,
    callEntry :: String
}  | ProcedureEntry String

instance Show Assembly where
    show (Assembly opt l a ent) = if (opt == CAL) then (show opt) ++ " " ++ ent else (show opt) ++ " " ++ (show l) ++ " " ++ (show a)

type Generate a = a -> Integer -> SymTable -> Maybe [Assembly]

(+++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
a +++ b = (++) <$> a <*> b

genFactor :: Generate Factor
genFactor (FactorId id) lev symTable = case ent of
    Nothing -> Nothing
    Just e  -> case kind e of 
                    Variable -> Just [Assembly LOD (lev - (level e)) (addr e) ""]
                    Constant -> Just [Assembly LIT 0 (val e) ""]
    where
        ent = lookUp symTable id lev
genFactor (FactorInt x) lev symTable = Just [Assembly LIT 0 x ""]
genFactor (FactorExpr x) lev symTable = genExpr x lev symTable

genItem :: Generate Item
genItem (Item factor rest) lev symTable = case reverse rest of
    []       -> genFactor factor lev symTable
    (x : vs) -> genItem (Item factor (reverse vs)) lev symTable +++
                genFactor (snd x) lev symTable +++
                case fst x of
                    '*' -> Just [Assembly OPR 0 4 ""]
                    '/' -> Just [Assembly OPR 0 5 ""]

genExpr :: Generate Expr
genExpr (Expr mark item rest) lev symTable = case reverse rest of
    []       -> genItem item lev symTable +++ Just (if (isNeg mark) then [Assembly OPR 0 1 ""] else [])
    (x : vs) -> genExpr (Expr mark item (reverse vs)) lev symTable +++
                genItem (snd x) lev symTable +++
                case fst x of
                    '+' -> Just [Assembly OPR 0 2 ""]
                    '-' -> Just [Assembly OPR 0 3 ""]
                         
genAssign :: Generate Assign
genAssign (Assign id expr) lev symTable = case lookUp symTable id lev of
        Nothing -> Nothing
        Just e  -> genExpr expr lev symTable +++ Just [Assembly STO (lev - (level e)) (addr e) ""]

genCondition :: Generate Condition
genCondition (Odd expr) lev symTable = genExpr expr lev symTable +++ Just [Assembly OPR 0 6 ""]
genCondition (Condition e1 opt e2) lev symTable = 
    genExpr e1 lev symTable +++
    genExpr e2 lev symTable +++
    case opt of
        "="  -> Just [Assembly OPR 0 8 ""]
        "<>" -> Just [Assembly OPR 0 9 ""]
        "<"  -> Just [Assembly OPR 0 10 ""]
        ">=" -> Just [Assembly OPR 0 11 ""]
        ">"  -> Just [Assembly OPR 0 12 ""]
        "<=" -> Just [Assembly OPR 0 13 ""]

genProcedureCall :: Generate ProcedureCall
genProcedureCall (ProcedureCall f) lev symTable = Just [Assembly CAL 0 0 f]

genComRead :: Generate ComRead
genComRead (ComRead ids) lev symTable = case ids of
    []          -> Just []
    (id : vs) -> case lookUp symTable id lev of
        Nothing -> Nothing
        Just e  -> Just [Assembly RED (lev - (level e)) (addr e) ""] +++ genComRead (ComRead vs) lev symTable

genComWrite :: Generate ComWrite
genComWrite (ComWrite exprs) lev symTable = case exprs of
    []          -> Just []
    (expr : vs) -> genExpr expr lev symTable +++ Just [Assembly WRT 0 0 ""] +++ genComWrite (ComWrite vs) lev symTable

genCommand :: Generate Command
genCommand (CAssign ass) lev symTable = genAssign ass lev symTable
genCommand (CCall   cal) lev symTable = genProcedureCall cal lev symTable
genCommand (CRead   red) lev symTable = genComRead red lev symTable
genCommand (CWrite  wrt) lev symTable = genComWrite wrt lev symTable
genCommand (CSome   som) lev symTable = case som of
                                            []       -> Just []
                                            (x : vs) -> genCommand x lev symTable +++ genCommand (CSome vs) lev symTable
genCommand (CIf con com) lev symTable =
    case (c1, c2) of
        (Just a1, Just a2) -> Just $ a1 ++ [Assembly JPC 0 (toInteger (length a2 + 1)) ""] ++ a2
        _                  -> Nothing
    where
        c1 = genCondition con lev symTable
        c2 = genCommand com lev symTable
genCommand (CWhile con com) lev symTable = 
    case (c1, c2) of
        (Just a1, Just a2) -> Just $ a1 ++ [Assembly JPC 0 (toInteger (length a2 + 2)) ""] ++ a2 
                              ++ [Assembly JMP 0 (toInteger (0 - (length a1 + length a2 + 1))) ""]
        _                  -> Nothing
    where
        c1 = genCondition con lev symTable
        c2 = genCommand com lev symTable

genSubProgram :: Generate SubProgram
genSubProgram (SubProgram constState varState procedures command) lev symTable = 
    Just [Assembly INT 0 (toInteger (length varState)) ""] +++
    genCommand command lev symTable +++ 
    Just [Assembly OPR 0 0 ""]


