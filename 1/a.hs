import Data.List (transpose, sort)

f = sum . uncurry (zipWith ((abs .) . (-))) . (\[x, y] -> (x, y)) . map sort
main = getContents >>= print . f . transpose . map (map read . words) . lines 
