import Data.Matrix
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad
instance Num (Int, Int) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) - (c, d) = (a - c, b - d)
ne p = map (+p) [(0, 1), (0, -1), (1, 0), (-1, 0)]
is n m (i, j) = 1 <= i && i <= n && 1 <= j && j <= m
bfs p is le = go [p] (S.fromList [p])
  where
    go [] s = s
    go (v:q) s = let ws = filter (\w -> is w && le ! w == le ! p && w `S.notMember` s) (ne v)
                 in  go (q ++ ws) (foldr S.insert s ws)
pe le is k = sum <$> (mapM f $ filter (\z -> not (is z && le ! z == le ! k)) (ne k))
  where
    f z = let d = z - k
          in modify (M.insertWith (<>) d (S.singleton z)) >> return 1
bfs2 ps p = go [p]
  where
    go [] s = s
    go (v:q) s | v `S.member` s = go q s
               | otherwise = go (q ++ filter (`S.member` ps) (ne v)) (S.insert v s)
sides = foldr go 0
  where
    go poss k  = k + (sum $ evalState (mapM (g poss) (S.toList poss)) S.empty)
    g poss pos = do
      s <- get
      if pos `S.member` s then return 0
      else put (bfs2 poss pos s) >> return 1
solve ma = let cs = toList (mapPos const ma)
           in snd $ foldr f (S.empty, (0, 0)) cs
  where
    is' = is (nrows ma) (ncols ma)
    f c s@(md, _) | c `S.member` md = s
    f c (md, (s1, s2)) = let vs = bfs c is' ma
                             a = S.size vs :: Int
                             (peris, sus) = runState (mapM (pe ma is') (S.toList vs)) M.empty
                             sid = sides (M.elems sus)
                         in  (md <> vs, (s1 + a * sum peris, s2 + sid * a))
main = getContents >>= print . solve . fromLists . lines