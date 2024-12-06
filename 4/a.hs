import Data.Matrix
g = uncurry . flip (flip . safeGet)
z m l | Just "XMAS" <- sequence (map (g m . l) [0..3]) = 1
z _ _ = 0
t m i j = sum . map (z m) $ [\k -> (i + z * k, j + q * k) | z <- [-1..1], q <- [-1..1], (z, q) /= (0, 0)]
f a = sum (map (uncurry (t a)) [(i, j) | i <- [1 .. nrows a], j <- [1 .. ncols a]])
main = getContents >>= print . f . fromLists . lines
