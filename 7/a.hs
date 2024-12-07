import Control.Arrow
s = ((read *** (reverse . map read . words . tail)) .) . span . (/=)
f 0 [] = True
f k (x:xs) | f (k - x) xs = True
					 | (q, 0) <- quotRem k x = f q xs
f _ _ = False
main = getContents >>= print . sum . map fst . filter (uncurry f) . map (s ':') . lines
