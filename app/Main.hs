module Main where

import ParserCombinator
import Control.Applicative
import StackMachine
import CFG
import Parser
import Text.Pretty.Simple (pPrint)

testExpr :: String -> Integer
testExpr s = case runParser exprP s of
                Nothing     -> 0
                Just (a, b) -> let (x, y, z) = (unStOut (evalExpr b [])) []
                               in x
                               
main :: IO ()
main = do
    s <- readFile "PL0_code.in"
    print (runParser subProgramP s)
    s <- readFile "PL0_code0.in"
    print (runParser subProgramP s)
    s <- readFile "PL0_code1.in"
    print (runParser subProgramP s)
    s <- readFile "PL0_code2.in"
    print (runParser subProgramP s)
    s <- readFile "PL0_code3.in"
    print (runParser subProgramP s)
