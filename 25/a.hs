import qualified Data.List.Split as SP
import Data.List

f l r [] = (l, r)
f l r (xs@(('#':_):_):xxs)= f (map (subtract 1 . length . takeWhile (== '#')) xs:l) r xxs
f l r (xs@(('.':_):_):xxs) = f l (map ((6 -) . length . takeWhile (== '.')) xs:r) xxs
g ls ks = length [1 | l <- ls, k <- ks, fits l k]
  where
    fits l k = all id (zipWith (\li ki -> (7 - li - 1) >= ki + 1) l k)
main = getContents >>= print . uncurry g . f [] [] . map transpose . SP.splitOn [""] . lines