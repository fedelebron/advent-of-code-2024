import Control.Arrow
s = ((read *** (reverse . map read . words . tail)) .) . span . (/=)
f :: Integer -> [Integer] -> Integer
f n xs = if go n xs then n else 0
 where
  go 0 [] = True
  go k (x:xs) | go (k - x) xs = True
              | (q, 0) <- quotRem k x = go q xs
  go _ _ = False
main = getContents >>= print . sum . map (uncurry f . s ':') . lines
