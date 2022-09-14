module CodeGen where

import CFG
import SymbolTable

data Opt = LIT | LOD | STO | CAL | INT | JMP | JPC | OPR | RED | WRT
    deriving (Show, Eq)

data Assembly = Assembly {
    fieldOpt  :: Opt,
    fieldL    :: Integer,
    fieldA    :: Integer,
    callEntry :: String
}  | ProcedureEntry ProcedureHead

instance Show Assembly where
    show (Assembly opt l a ent) = if (opt == CAL) then (show opt) ++ " <" ++ ent ++ ".> " ++ (show l) ++ " " ++ (show a) else (show opt) ++ " " ++ (show l) ++ " " ++ (show a)
    show (ProcedureEntry (ProcedureHead p)) = p ++ "."

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
    []       -> genItem item lev symTable +++ Just (if (odd (length ((filter (== '-') mark)))) then [Assembly OPR 0 1 ""] else [])
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
genProcedureCall (ProcedureCall f) lev symTable = case lookUp symTable f lev of
                                                    Nothing -> Nothing -- Never could happen
                                                    Just e  -> Just [Assembly CAL (lev - (level e)) 0 f]

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

getName :: ProcedureHead -> String
getName (ProcedureHead s) = s

genSubProgram :: String -> Generate SubProgram
genSubProgram name (SubProgram constState varState procedures command) lev symTable = 
    (foldl (+++) (Just []) [genSubProgram (getName (fst (procedures !! i))) (snd (procedures !! i)) (lev + 1) symTable | i <- [0 .. (length procedures - 1)]]) +++
    Just [ProcedureEntry (ProcedureHead name)] +++
    Just [Assembly INT 0 (toInteger (length varState + 3)) ""] +++
    genCommand command lev symTable +++ 
    Just [Assembly OPR 0 0 ""]

fillBackT :: [Assembly] -> Integer -> SymTable -> SymTable
fillBackT x n t = case x of
    []       -> t
    (v : vs) -> case v of
        ProcedureEntry (ProcedureHead p) -> case lookUp t p 100 of
            Nothing -> t -- Never gonna happen
            Just e  -> fillBackT vs n (replace t e (Entry p Procedure 0 (level e) n))
        _           -> fillBackT vs (n + 1) t

fillBack :: [Assembly] -> Integer -> SymTable -> [Assembly]
fillBack x n t = case x of
    []       -> []
    (v : vs) -> case v of
        ProcedureEntry p     -> (v : (fillBack vs n t))
        Assembly opt l a ent -> case opt of
            JPC              -> ((Assembly opt l (n + a) ent) : (fillBack vs (n + 1) t))
            JMP              -> ((Assembly opt l (n + a) ent) : (fillBack vs (n + 1) t))
            CAL              -> case lookUp t ent 100 of
                                    Nothing -> [] -- Never gonna happen
                                    Just e  -> ((Assembly opt l (addr e) ent) : (fillBack vs (n + 1) t))
            _                -> (v : (fillBack vs (n + 1) t))

mainEntry :: [Assembly] -> Integer -> Integer
mainEntry xs n = 
    case xs of
        []       -> 0 -- Never gonna happen
        (v : vs) -> case v of
                        (ProcedureEntry (ProcedureHead p)) -> if (p == "main") then n else mainEntry vs n
                        _                                  -> mainEntry vs (n + 1)

prettyPrint :: [Assembly] -> Integer -> String
prettyPrint x n = case x of
    []       -> ""
    (v : vs) -> case v of
        ProcedureEntry p -> "<" ++ show v ++ ">\n" ++ 
                            prettyPrint vs n
        _                -> "  " ++ (show n) ++ ":  " ++ (show v) ++ "\n" ++
                            prettyPrint vs (n + 1)

prettyPrintT :: SymTable -> String
prettyPrintT x = case x of
    []       -> ""
    (v : vs) -> name v ++ " " ++ show (kind v) ++ " " ++ show (val v) ++ " " ++ show (level v) ++ " " ++ show (addr v) ++ "\n" ++
                prettyPrintT vs