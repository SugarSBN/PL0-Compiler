module Main where

import ParserCombinator
import Control.Applicative
import CFG
import Parser
import SymbolTable
import CodeGen
import Text.Pretty.Simple (pPrint)

                               
main :: IO ()
main = do
    {-
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
    -}
    s <- readFile "./testfiles/PL0_code_gcd.pas"
    --print (runParser subProgramP s)
    pPrint $ (runParser subProgramP s) >>= genSymTable >>= (genFactor (FactorId "y") 0)
