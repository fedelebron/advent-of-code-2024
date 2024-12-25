import qualified Data.Map as M
import Control.Arrow
import Data.Bits
import Z3.Monad
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Loops
import Data.List (intercalate)
import Text.Printf

data Op = AND | OR | XOR deriving (Show, Read)
data ExprF = ExprF String Op String deriving Show
instance Eq (Z3 AST) where
  (==) = error "ugh"

instance Bits (Z3 AST) where
  (.&.) = (join .) . liftM2 mkBvand
  (.|.) = (join .) . liftM2 mkBvor
  xor = (join .) . liftM2 mkBvxor

evaluate inputs gates swaps = go where
  go x | Just v <- M.lookup x inputs = v
       | let x' = M.findWithDefault x x swaps,
         Just (ExprF a op b) <- M.lookup x' gates =
          let a' = go a
              b' = go b
          in case op of
            AND -> a' .&. b'
            OR -> a' .|. b'
            XOR -> a' `xor` b'
outs = filter ((=='z') . head) . M.keys
s c x = let (a, _:b) = span (/= c) x
        in (a, b)
pl xs = let [var, val] = words xs
        in  (init var, read val :: Int)
pg xs = let [x, g, y, _, z] = words xs
        in  (z, ExprF x (read g) y)
f inputs gates = let o = outs gates
                     e = evaluate inputs gates M.empty
                 in foldr (\a b -> 2*b+a) 0 (map e o)
si = printf "%02d"  
showSwaps m = intercalate "," (S.toList (M.keysSet m))
cantBreak z e i = local $ do
  zi <- e ('z':si i)
  mkExtract i i z >>= mkEq zi >>= mkNot >>= assert
  (== Unsat) <$> solverCheck
g inputs gates swaps = evalZ3 $ do
    [x, y, z] <- mapM (flip mkFreshBvVar 45) (words "x y z")
    mkBvadd x y >>= mkEq z >>= assert
    let eb p v i = (p ++ si i, ) <$> mkExtract i i v
    bx <- M.fromList <$> mapM (eb "x" x) [0 .. 44]
    by <- M.fromList <$> mapM (eb "y" y) [0 .. 44]
    let e = evaluate (fmap return (bx `M.union` by)) gates swaps
    z <- dropWhileM (cantBreak z e) [0 .. 44]
    case z of
      (i:_) -> return (Right i)
      []    -> return (Left (showSwaps swaps))
dfs inputs gates = go
  where
    go swaps x | x `S.member` (M.keysSet inputs) = S.empty
               | otherwise = S.union (S.fromList [x, x'])
                             (S.union (go M.empty a1) (go M.empty a2))
      where
        x' = M.findWithDefault x x swaps
        Just (ExprF a1 _ a2) = M.lookup x' gates
fs inputs gates i swaps = let a = S.toList $ des ('z': si i)
                              b = S.toList $ des "z45"
                          in go (liftM2 (,) a b)
  where
    des = dfs inputs gates swaps
    isCorrect = des ('z':si (i - 1))
    go ((c1, c2):xs)
      | (c1 `S.member` isCorrect || c2 `S.member` isCorrect || c1 `S.member` des c2) = go xs
      | otherwise = let s = M.insert c1 c2 (M.insert c2 c1 swaps)
                    in do
                      rr <- g inputs gates s
                      case rr of
                        Right j | j > i -> fs inputs gates j s
                                | otherwise -> go xs
                        Left s -> return s
h (xs, ys) = do
  print (f xs ys)
  rr <- g xs ys M.empty
  case rr of
    Left s -> print s
    Right i -> fs xs ys i M.empty >>= print
main = getContents >>= h . (M.fromList . map pl *** M.fromList . map pg) . s "" . lines