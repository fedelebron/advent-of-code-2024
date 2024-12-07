import Control.Arrow
import Data.List
s = ((read *** (reverse . map read . words . tail)) .) . span . (/=)
f n xs = if go n xs then n else 0
 where
  go 0 [] = True
  go k (x:xs) | go (k - x) xs = True
              | (q, 0) <- quotRem k x, go q xs = True
              | k > 0,
                k' <- show k,
                x' <- show x,
                x' `isSuffixOf` k' = go (read (take (length k' - length x') k')) xs
  go _ _ = False
main = getContents >>= print . sum . map (uncurry f . s ':') . lines
