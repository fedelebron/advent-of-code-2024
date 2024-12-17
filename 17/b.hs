import Z3.Monad
import Data.Maybe
import Control.Monad

{-
  while (A) {
    ull B = A % 8;
    B ^= 1;
    ull C = A >> B;
    A >>= 3;
    B ^= 4;
    B ^= C;
    stc::cout << (B % 8) << std::endl;
-}
program = [2,4,1,1,7,5,0,3,1,4,4,4,5,5,3,0]
iConst k = mkInteger k >>= mkInt2bv 64
oneRound (a, cs@[c1, c3, c4, c7], res) _ = do
  b <- a `mkBvand` c7
  b <- b `mkBvxor` c1
  bs <- mkBv2int b False
  c <- a `mkBvlshr` b
  a <- a `mkBvlshr` c3
  b <- b `mkBvxor` c4
  b <- b `mkBvxor` c
  r <- b `mkBvand` c7
  return (a, cs, res ++ [r])

allRounds a = do
  constants <- sequence (map iConst [1, 3, 4, 7])
  (a', _, outs) <- foldM oneRound (a, constants, []) [1 .. 16]
  targets <- mapM (mkInteger >=> mkInt2bv 64) program
  z0 <- iConst 0
  mkAnd =<< zipWithM mkEq (z0:outs) (a':targets)

problem lim = do
  a <- mkFreshBvVar "a" 64
  case lim of 
    Nothing -> return ()
    Just x -> do
      y <- iConst x
      assert =<< mkBvult a y
  assert =<< allRounds a
  fmap snd $ withModel $ \m -> fromJust <$> evalInt m a

bestSol problem = go Nothing
  where
    go z = do
      sols <- evalZ3 (problem z)
      case sols of
        Just x -> putStrLn ("Found " ++ show x ++ ", excluding...") >> go (Just x)
        Nothing -> putStrLn ("Minimum solution: " ++ show z)
      
main :: IO ()
main = bestSol problem