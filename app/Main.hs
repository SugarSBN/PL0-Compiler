module Main where

import Control.Applicative
import qualified Text.Parsec as P
import CFG
import Parser
import SymbolTable
import CodeGen
import StackMachine
import System.Environment

unFold :: Maybe a -> a
unFold (Just t) = t

main :: IO ()
main = do
    fileName <- getArgs
    -- s <- readFile "./testfiles/PL0_code.pas"
    -- s <- readFile "./testfiles/PL0_code0.pas"
    -- s <- readFile "./testfiles/PL0_code1.pas"
    -- s <- readFile "./testfiles/PL0_code2.pas"
    -- s <- readFile "./testfiles/PL0_code3.pas"
    -- s <- readFile "./testfiles/PL0_code4.pas"
    -- s <- readFile "./testfiles/PL0_code_gcd.pas"
    s <- readFile (head fileName)
    case P.parse subProgram "" s of                                  
        Left e -> do putStrLn "Error parsing input:"
                     print e                                         -- Parsing,                p    :: SubProgram
        Right p -> do let t = symSubProgram p 0                      -- Symbol Table,           t    :: SymTable
                      let ass = unFold (genSubProgram "main" p 0 t)  -- Assembly,               ass  :: [Assembly]
                      let t' = fillBackT ass 0 t                     -- Fill back symbol table, t'   :: SymTable
                      let ass' = fillBack ass 0 t'                   -- Fill back assembly,     ass' :: [Assembly]
                      let pc = mainEntry ass' 0
                      -- print pc
                      -- print (runParser subProgramP s)
                      writeFile "output.asm" ((prettyPrintT t') ++ "\n" ++ (prettyPrint ass' 0))
                      (runAssembly ass' (State [0, 0, 0] 0 pc)) >>= (\a -> print a)
                      -- (runAssemblyStep ass' 22 (State [0, 0, 0] 0 pc)) >>= (\a -> print a)
