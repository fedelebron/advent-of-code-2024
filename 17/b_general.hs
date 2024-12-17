import Z3.Monad
import Data.Maybe
import Control.Monad

iConst = mkInteger >=> mkInt2bv 64
-- We assume
-- * every program ends with the jump (3, 0)
-- * there are no other jumps (opcode 3)
-- * the jump exits the program after exactly n rounds, n the argument of manyRounds
-- * every round prints exactly one thing
oneRound (opcode:operand:prog) t@(a, b, c, cs, res) = case (opcode, fromInteger operand) of
  (0, x) -> do
    a <- a `mkBvlshr` (combo x)
    oneRound prog (a, b, c, cs, res)
  (1, x) -> do
    b <- (b `mkBvxor` (cs !! x))
    oneRound prog (a, b, c, cs, res)
  (2, x) -> do
    b <- combo x `mkBvand` (cs !! 7)
    oneRound prog (a, b, c, cs, res)
  (3, 0) -> return t
  (4, _) -> do
    b <- b `mkBvxor` c
    oneRound prog (a, b, c, cs, res)
  (5, x) -> do
    r <- (combo x) `mkBvand` (cs !! 7)
    oneRound prog (a, b, c, cs, res ++ [r])
  (6, x) -> do
    b <- a `mkBvlshr` (combo x)
    oneRound prog (a, b, c, cs, res)
  (7, x) -> do
    c <- a `mkBvlshr` (combo x)
    oneRound prog (a, b, c, cs, res)
  where
    combo x | 0 <= x && x <= 3 = cs !! x
    combo 4 = a
    combo 5 = b
    combo 6 = c

manyRounds n program a = do
  constants <- sequence (map iConst [0 .. 7])
  b <- iConst 0
  c <- iConst 0
  (a', _, _, _, outs) <- foldM (const . oneRound program) (a, b, c, constants, []) [1 .. n]
  targets <- mapM iConst program
  z0 <- iConst 0
  mkAnd =<< zipWithM mkEq (z0:outs) (a':targets)

problem allRounds lim = do
  a <- mkFreshBvVar "a" 64
  maybe (return ()) (((assert =<<) . mkBvult a =<<) . iConst) lim
  assert =<< allRounds a
  fmap snd $ withModel $ \m -> fromJust <$> evalInt m a

bestSol problem = go Nothing
  where
    go z = do
      sols <- evalZ3 (problem z)
      case sols of
        Just x -> putStrLn ("Found " ++ show x ++ ", excluding...") >> go (Just x)
        Nothing -> putStrLn ("Minimum solution: " ++ show z)
      
main = let program = [2,4,1,1,7,5,0,3,1,4,4,4,5,5,3,0]
       in  bestSol (problem (manyRounds (length program) program))