import Data.Matrix
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split as SP
import Z3.Monad
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Debug.Trace
mkEdges :: [[String]] -> (Matrix Int, M.Map Int [Int], M.Map String Int, [(Int, Int)])
mkEdges = go M.empty []
  where
    go m es [] = let n = length (M.keysSet m)
                     ma = foldr (\(a, b) -> unsafeSet 1 (a+1, b+1)) (zero n n) es
                     g :: M.Map Int [Int] = foldr (flip M.insert []) M.empty (M.elems m)
                     g' :: M.Map Int [Int] = foldr (\(a, b) -> M.adjust (b:) a) g es
                 in  (ma, g', m, es)
    go m es ([a, b]:xs) = let (a', m') = f a m
                              (b', m'') = f b m'
                          in go m'' ((a', b'):(b', a'):es) xs
    f a m | Just r <- M.lookup a m = (r, m)
          | otherwise = let r = M.size m in (r, M.insert a r m)
g (mat, g, m, es) = let r = [S.fromList [u, v, w] | ('t':_, u) <- M.assocs m, v <- M.findWithDefault [] u g, w <- M.findWithDefault [] v g, mat ! (w+1, u+1) == 1]
                    in  length (S.fromList r)
h (mat, g, m, es) = let n = S.size (M.keysSet g)
                        rm = M.fromList [(v, k) | (k, v) <- M.assocs m]
                    in evalZ3 $ do
                      zero <- mkInteger 0
                      one <- mkInteger 1
                      vs <- mapM (mkFreshBoolVar . ("v_" ++) . show) [0 .. n - 1]
                      vsi <- mapM (\x -> mkIte x one zero) vs
                      z <- mkFreshIntVar "z"
                      mkAdd vsi >>= mkEq z >>= optimizeAssert
                      forM_ (zip [0..] vs) $ \(i, vi) -> do
                        forM_ (zip [0 .. i -1] vs) $ \(j, vj) -> do
                          when (mat ! (i + 1, j + 1) == 0) $ do
                            mkAnd [vi, vj] >>= mkNot >>= optimizeAssert
                      z' <- optimizeMaximize z
                      Sat <- optimizeCheck []
                      m <- optimizeGetModel
                      z'' <- optimizeGetUpper z' >>= evalInt m
                      vs' :: [Bool] <- catMaybes <$> mapM (evalBool m) vs
                      let vs'' = intercalate "," . sort . catMaybes <$> map (flip M.lookup rm . fst) . filter snd $ zip [0..] vs'
                      return (z'', vs'')
s (gr, hr) = do
  print gr
  hr >>= print
main = getContents >>= s . (g &&& h). mkEdges . map (SP.splitOn "-") . lines