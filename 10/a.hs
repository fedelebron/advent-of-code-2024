import Data.Matrix
import qualified Data.Set as S
import Control.Arrow ((&&&))
instance Num (Int, Int) where
  (a, b) + (c, d) = (a + c, b + d)
ds = [(0, 1), (0, -1), (1, 0), (-1, 0)]
f a b xs = sum . map (go 0 . a . return) . filter (( == 0) . (m !)) . toList . mapPos const $ m
  where
    m = fromLists xs
    v (i, j) = 1 <= i && i <= nrows m && 1 <= j && j <= ncols m
    go 9 xs = length xs
    go k xs = go (k + 1) (a [x' | x <- b xs, d <- ds, let x' = x + d, v x', m ! x' == k + 1])
main = getContents >>= print . (f S.fromList S.toList &&& f id id) . map (map (read . return)) . lines 
