import Data.Matrix
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.MemoTrie
import Control.Arrow
instance Num (Int, Int) where
  (a, b) + (c, d) = (a + c, b + d)
ds = [((0, 1), '>'), ((0, -1), '<'), ((-1, 0), '^'), ((1, 0), 'v')]
makeGraph cs r c = let assocs = filter ((/= 'X') . snd) (toList (mapPos (,) (fromList r c cs)))
                       rev = M.fromList assocs
                       m = M.fromList (map swap assocs)
                   in M.fromList [(v, [(rr, k) | (d, k) <- ds, Just rr <- [M.lookup (vc + d) rev]]) | (v, vc) <- M.toList m]
nums = makeGraph "789456123X0A" 4 3
dirs = makeGraph "X^A<v>" 2 3
gs = [nums, dirs]
bfs = memo3 bfs'
  where
    bfs' v w gi = let gg = gs !! gi
               in  go gg [(v, "")] Nothing []
      where
        go _ [] _ bs = bs
        go g q@((u, pa):qs) Nothing bs | u == w = go g q (Just (length pa)) []
        go g ((u, pa):qs) (Just b) bs | u == w, b /= length pa = bs
        go g ((u, pa):qs) mb bs
          | Just cds <- M.lookup u g = let ns = [(c, pa ++ [d]) | (c, d) <- cds]
                                           bs' = case (u == w, mb) of
                                            (True, Just b) | b == length pa -> bs ++ [pa ++ "A"]
                                            otherwise -> bs
                                       in  go g (qs ++ ns) mb bs'
f :: ((Char, Char), (Int, Int)) -> Int -- grotesque type since MemoTrie only supports up to 3-tuples
f = memo f'
  where
    f' ((x, y), (k, i)) = let pas = bfs x y i
                      in if k == 0
                          then minimum (map length pas)
                          else minimum [g pa (k - 1) 1 | pa <- pas]
g pa k gi = let s = 'A':pa
            in  sum [f ((x, y), (k, gi)) | (x, y) <- zip s (tail s)]
solve l lines = sum [(g li l 0 * read (init li)) | li <- lines]
main = getContents >>= print . (solve 2 &&& solve 25) . lines