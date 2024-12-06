import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.List
h xs = case filter (\(a:_ ,b:_) -> not (1 <= b - a && b - a <= 3)) . init $ zip (tail (reverse (tails (reverse xs)))) (tail (tails xs)) of
         (x, y):_ -> Just (reverse x, y)
         otherwise -> Nothing
f xs = case h xs of
  Nothing -> True
  Just (r, q) -> let a = r ++ tail q
                     b = init r ++ q
                 in isNothing (h a) || isNothing (h b)
g = f . reverse
main = getContents >>= print . length . filter (liftM2 (||) f g) . map (map read . words) . lines
