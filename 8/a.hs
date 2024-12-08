import qualified Data.Set as S
import qualified Data.Map as M
l x y n m = let (xi, xj) = x
                (yi, yj) = y
                di = xi - yi
                dj = xj - yj
                i = xi + di
                j = xj + dj
            in if 0 <= i && i < n && 0 <= j && j < m
               then [(i, j)]
               else []
parse ls = let n = length ls
               m = length (head ls)
               cs = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
               mm = zip (concat ls) cs
               ns = M.fromListWith (++) [(c, [p]) | (c, p) <- mm, c /= '.']
           in (n, m, ns)
solve ls = let (n, m, ns) = parse ls
           in print . length $ S.fromList [r | t <- M.elems ns, x <- t, y <- t, x /= y, r <- l x y n m]
main = getContents >>= solve . lines
