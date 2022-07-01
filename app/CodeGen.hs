module CodeGen where

import CFG
import SymbolTable
import Tools

data Opt = LIT | LOD | STO | CAL | INT | JMP | JPC | OPR
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
    Just e  -> Just [Assembly LOD ((level e) - lev) (addr e) ""]
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
                         

genSubProgram :: SubProgram -> Integer -> Maybe [Assembly]
genSubProgram (SubProgram constState varState procedures command) lev = Just $ 
    [Assembly INT 0 (toInteger (length varState)) ""] ++
    [Assembly OPR 0 0 ""]


