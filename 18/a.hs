import qualified Data.List.Split as SP
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Set as S
parse lis = let f x = let [a, b] = SP.splitOn "," x in (read a, read b)
                xys = map f lis
            in (M.fromList (zip xys [0..]), V.fromList xys)
height = 71
width = 71
valid i j = 0 <= i && i < height && 0 <= j && j < width
neighbors (i, j) = filter (uncurry valid) [(i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1)]
bfs k m = let v = (0, 0)  
          in  go [v] S.empty (S.singleton v) (M.singleton v 0)
  where
    end = (height - 1, width - 1)
    blocked (i, j) | Just k' <- M.lookup (i, j) m = k' <= k
                   | otherwise = False
    go [] popped added dist = Nothing
    go (v:vs) popped added dist
      | v == end = M.lookup v dist
      | otherwise = let ws = filter (not . blocked) $ filter (`S.notMember` added) (neighbors v)
                        Just d = M.lookup v dist
                    in  go (vs++ws) (S.insert v popped) (foldr S.insert added ws) (foldr (flip M.insert (d + 1)) dist ws)              
g m v = go 0
  where
    go k | Nothing <- bfs k m = v ! k
    go k = go (k + 1)
main = getContents >>= print . (uncurry (const . bfs 1024) &&& uncurry g) . parse . lines