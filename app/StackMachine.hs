module StackMachine where

import CodeGen

data State = State {
    stack :: [Integer],
    frame :: Integer,
    pc    :: Integer
} deriving (Show)

getLocation :: [Integer] -> Integer -> Integer -> Integer -> Integer
getLocation st fp l a = case l of
                            0 -> fp + a
                            _ -> getLocation st (readStack st fp) (l - 1) a

writeStack :: [Integer] -> Integer -> Integer -> [Integer]
writeStack st p new = case p of
                            0 -> new : (tail st)
                            _ -> (head st) : (writeStack (tail st) (p - 1) new)

readStack :: [Integer] -> Integer -> Integer
readStack st p = (st !! (fromInteger p))

popStack :: [Integer] -> Integer -> [Integer]
popStack st p = case p of
                    0 -> []
                    _ -> (head st) : (popStack (tail st) (p - 1))

runCode :: Assembly -> State -> IO State
runCode (Assembly LIT l a ent) (State st fp pc) = return $ State (st ++ [a]) fp (pc + 1)
runCode (Assembly LOD l a ent) (State st fp pc) = do
                                                    let pos = getLocation st fp l a
                                                    let red = readStack st pos
                                                    return $ State (st ++ [red]) fp (pc + 1)
runCode (Assembly STO l a ent) (State st fp pc) = do
                                                    let pos = getLocation st fp l a
                                                    let st' = writeStack st pos (head (reverse st))
                                                    return $ State st' fp (pc + 1)
runCode (Assembly RED l a ent) (State st fp pc) = do
                                                    s <- getLine
                                                    let x = (read s) :: Integer
                                                    let pos = getLocation st fp l a
                                                    let st' = writeStack st pos x
                                                    return $ State st' fp (pc + 1)
runCode (Assembly WRT l a ent) (State st fp pc) = do
                                                    print $ (head (reverse st))
                                                    return $ State st fp (pc + 1) 
runCode (Assembly CAL l a ent) (State st fp pc) = do
                                                    let ra = pc + 1
                                                    let dl = fp
                                                    let sl = getLocation st fp l 0
                                                    return $ State (st ++ [sl, dl, ra]) fp a
runCode (Assembly INT l a ent) (State st fp pc) = do
                                                    let fp' = toInteger (length st - 3)
                                                    let var = [0 | i <- [1 .. (a - 3)]]
                                                    return $ State (st ++ var) fp' (pc + 1)
runCode (Assembly JMP l a ent) (State st fp pc) = do
                                                    return $ State st fp a
runCode (Assembly JPC l a ent) (State st fp pc) = do
                                                    return $ if ((head (reverse st)) == 0) then State st fp a else State st fp (pc + 1)
runCode (Assembly OPR 0 0 ent) (State st fp pc) = do
                                                    let fp' = readStack st (fp + 1)
                                                    let pc' = readStack st (fp + 2)
                                                    return $ State (popStack st fp) fp' pc'
runCode (Assembly OPR 0 1 ent) (State st fp pc) = do
                                                    return $ State (writeStack st (toInteger (length st - 1)) (- (head (reverse st)))) fp (pc + 1)
runCode (Assembly OPR 0 2 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (op1 + op2)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 3 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (op2 - op1)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 4 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (op2 * op1)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 5 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (div op2 op1)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 6 ent) (State st fp pc) = do
                                                    return $ State (writeStack st (toInteger (length st - 1)) (mod (head (reverse st)) 2)) fp (pc + 1)
runCode (Assembly OPR 0 7 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (mod op2 op1)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 8 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op1 == op2) then 1 else 0)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 9 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op1 == op2) then 0 else 1)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 10 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op2 < op1) then 1 else 0)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 11 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op2 >= op1) then 1 else 0)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 12 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op2 > op1) then 1 else 0)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 
runCode (Assembly OPR 0 13 ent) (State st fp pc) = do
                                                    let op1 = head (reverse st)
                                                    let op2 = head (tail (reverse st))
                                                    let st' = writeStack st (toInteger (length st - 2)) (if (op2 <= op1) then 1 else 0)
                                                    return $ State (popStack st' (toInteger (length st' - 1))) fp (pc + 1) 

findCode :: [Assembly] -> Integer -> Assembly
findCode xs n = case n of
                    0 -> case head xs of
                            ProcedureEntry _ -> findCode (tail xs) 0
                            _                -> head xs
                    _ -> case head xs of
                            ProcedureEntry _ -> findCode (tail xs) n
                            _                -> findCode (tail xs) (n - 1)

runAssembly :: [Assembly] -> State -> IO State
runAssembly xs st = case (stack st) of
                        [] -> return st
                        _  -> let code = findCode xs (pc st) in
                                (runCode code st) >>= (runAssembly xs)

runAssemblyStep :: [Assembly] -> Integer -> State -> IO State
runAssemblyStep xs step st = case step of
                                0 -> return st
                                _  -> let code = findCode xs (pc st) in
                                        (runCode code st) >>= (runAssemblyStep xs (step - 1))
