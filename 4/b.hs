import Data.Matrix
g = uncurry . flip (flip . unsafeGet)
h = map . g
a = ["SAM", "MAS"]
t m | h m [(1, 1), (2, 2), (3, 3)] `elem` a && h m [(3, 1), (2, 2), (1, 3)] `elem` a = 1
t _ = 0
f a = sum . map t $ [submatrix i (i + 2) j (j + 2) a | i <- [1 .. nrows a - 2], j <- [1 .. ncols a - 2]]
main = getContents >>= print. f . fromLists . lines
