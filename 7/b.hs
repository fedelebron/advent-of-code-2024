import Control.Arrow
import Data.List
s = ((read *** (reverse . map read . words . tail)) .) . span . (/=)
f 0 [] = True
f k (x:xs) | f (k - x) xs = True
					 | (q, 0) <- quotRem k x, f q xs = True
					 | k > 0,
					   k' <- show k,
						 x' <- show x,
						 x' `isSuffixOf` k' = f (read (take (length k' - length x') k')) xs
f _ _ = False
main = getContents >>= print . sum . map fst . filter (uncurry f) . map (s ':') . lines
