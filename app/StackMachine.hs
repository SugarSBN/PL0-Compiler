module StackMachine where
import CFG
import Tools

type Stack = [Integer]

newtype M a = StOut (Stack -> (a, Stack, String))

unStOut :: M a -> (Stack -> (a, Stack, String))
unStOut (StOut f) = f

instance Functor M where
    fmap mp (StOut f) = StOut $ \st -> 
                                    let 
                                        (a, st', s) = f st
                                    in (mp a, st', s)
                     
instance Applicative M where
    pure x = StOut $ \st -> (x, st, "")
    (StOut f1) <*> (StOut f2) = StOut $ \st ->
                                    let
                                        (a, st1, s1) = f1 st
                                        (b, st2, s2) = f2 st1
                                    in (a b, st2, s1 ++ s2)
                            

instance Monad M where
    return x = StOut $ \st -> (x, st, "")
    e >>= f  = StOut $ \st -> let 
                                   (a, st1, s1) = (unStOut e) st
                                   (b, st2, s2) = unStOut (f a) st1
                              in (b, st2, s1 ++ s2)


type Location = Integer
type Index = [String]

position :: String -> Index -> Location
position name index = let
                          pos n (nm : nms) = if name == nm then n else pos (n + 1) nms
                      in pos 1 index

fetch :: Location -> Stack -> Integer
fetch n (v : vs) = if n == 1 then n else fetch (n - 1) vs

put :: Location -> Integer -> Stack -> Stack
put n x (v : vs) = if n == 1 then (x : vs) else v : (put (n - 1) x vs)

getFrom :: Location -> M Integer
getFrom i = StOut $ \st -> (fetch i st, st, "")

write :: Location -> Integer -> M ()
write i v = StOut $ \st -> ((), put i v st, "")

push :: Integer -> M ()
push x = StOut $ \st -> ((), x : st, "")

pop :: M ()
pop = StOut $ \st -> let
                         (n : ns) = st
                     in ((), ns, "")


evalExpr :: Expr -> Index -> M Integer
evalExpr (Expr mark item rest) index = case rest of
                                            []       -> if (isNeg mark) then ((-) 0) <$> evalItem item index else evalItem item index
                                            (v : vs) -> if (isNeg mark) then (-) <$> evalExpr (Expr [fst v] (snd v) vs) index <*> evalItem item index 
                                                        else (+) <$> evalExpr (Expr [fst v] (snd v) vs) index <*> evalItem item index 


evalItem :: Item -> Index -> M Integer
evalItem (Item factor rest) index = case rest of
                                        []       -> evalFactor factor index
                                        (v : vs) -> case fst v of
                                            '*'  -> (*) <$> evalFactor factor index <*> evalItem (Item (snd v) vs) index
                                            '/'  -> div <$> evalFactor factor index <*> evalItem (Item (snd v) vs) index

evalFactor :: Factor -> Index -> M Integer
evalFactor factor index = case factor of
                            FactorId identifier -> let
                                                       (Identifier s) = identifier
                                                       loc = position s index
                                                   in getFrom loc
                            FactorInt v         -> return v
                            FactorExpr expr     -> evalExpr expr index


                            