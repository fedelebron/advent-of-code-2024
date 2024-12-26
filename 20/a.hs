import Data.Matrix
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
fi x m = head [p | (p, y) <- toList (mapPos (,) m), x == y]
neighbors (i, j) = [(i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)]
wall m i j = unsafeGet i j m == '#'
valid m i j = 1 <= i && i <= nrows m && 0 <= j && j <= ncols m
bfs mat fro to = go [fro] S.empty (S.singleton fro) (M.singleton fro 0)
  where
    go (v:vs) popped added dist
      | v == to = dist
      | otherwise = let good xy@(x, y) = not (wall mat x y) && valid mat x y && xy `S.notMember` added
                        ws = filter good (neighbors v)
                        Just d = M.lookup v dist
                    in  go (vs++ws) (S.insert v popped) (foldr S.insert added ws) (foldr (flip M.insert (d + 1)) dist ws) 
solve k m = length (S.fromList (f (M.toList fromStartDist)))
  where
    pos = fi 'S' m
    end = fi 'E' m
    fromStartDist = bfs m pos end
    toEndDist = bfs m end pos
    f xs = [(v, w) |
      (v@(vi, vj), d1) <- xs,
      Just d3 <- [M.lookup v toEndDist],
      i <- [vi - k .. vi + k],
      let ri = abs (vi - i),
      j <- [vj - (k - ri) .. vj + (k - ri)],
      valid m i j,
      let r = ri + abs (vj - j),
      let w = (i, j),
      Just d2 <- [M.lookup w toEndDist],
      r + d2 <= d3 - 100]
main = getContents >>= print . (solve 2 &&& solve 20). fromLists . lines