module Main where

import ParserCombinator
import Control.Applicative
import CFG
import Parser
import SymbolTable
import CodeGen

unFold :: Maybe a -> a
unFold (Just t) = t

main :: IO ()
main = do
    -- s <- readFile "./testfiles/PL0_code.pas"
    -- s <- readFile "./testfiles/PL0_code0.pas"
    -- s <- readFile "./testfiles/PL0_code1.pas"
    -- s <- readFile "./testfiles/PL0_code2.pas"
    -- s <- readFile "./testfiles/PL0_code3.pas"
    -- s <- readFile "./testfiles/PL0_code_gcd.pas"
    s <- readFile "./testfiles/PL0_code_primeTable.pas"
    let p = snd . unFold $ runParser subProgramP s -- Parsing,                p    :: SubProgram
    let t = symSubProgram p 0                      -- Symbol Table,           t    :: SymTable
    let ass = unFold (genSubProgram p 0 t)         -- Assembly,               ass  :: [Assembly]
    let t' = fillBackT ass 0 t                     -- Fill back symbol table, t'   :: SymTable
    let ass' = fillBack ass 0 t'                   -- Fill back assembly,     ass' :: [Assembly]
    -- print (runParser subProgramP s)
    writeFile "output.asm" ((prettyPrintT t') ++ "\n" ++ (prettyPrint ass' 0))
