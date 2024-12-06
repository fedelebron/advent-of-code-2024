import qualified Data.Set as S
import Data.Maybe
t (-1) 0 = (0, 1)
t 0 1 = (1, 0)
t 1 0 = (0, -1)
t 0 (-1) = (-1, 0)
p 'v' = (1, 0)
p '^' = (-1, 0)
p '>' = (0, 1)
p '<' = (0, -1)
syms = "^v><"
tp = S.map (\(i, j, _, _) -> (i, j))
f n m (ii, ij) (idi, idj) o = fmap (S.insert (ii, ij)) (go S.empty (ii, ij, idi, idj))
  where
    go se s@(i, j, di, dj) | (i + di, j + dj) `S.member` o = let (di', dj') = t di dj
                                                             in go' se (i, j, di', dj')
                           | otherwise = go' se (i + di, j + dj, di, dj)
    go' se s | s `S.member` se = Nothing
    go' se (i, j, _, _) | i < 0 || j < 0 || i >= n || j >= m = Just (tp se)
    go' se s = go (S.insert s se) s
parse ls = let n = length ls
               m = length (head ls)
               cs = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
               mm = zip cs (concat ls)
               os = S.fromList [c | (c, '#') <- mm]
               (s, d) = head [(c, p k) | (c, k) <- mm, k `elem` syms]
           in (n, m, s, d, os)
solve ls = let (n, m, s, d, os) = parse ls
               g = f n m s d
               se = fromJust (g os)
               cs = S.toList (S.filter (\x -> x /= s && x `S.notMember` os) se)
               ys = filter isNothing (map (g . flip S.insert os) cs)
           in print (length se, length ys)
main = getContents >>= solve . lines
