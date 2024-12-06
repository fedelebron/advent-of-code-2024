import Data.List
import Data.Maybe
import Control.Arrow ((&&&))
import qualified Data.Map as M
f [xs, ys] = let m = map (head &&& fromIntegral . length) . group . sort $ ys
             in  sum $ zipWith (*) xs (map (fromMaybe 0 . flip lookup m) xs)
main = getContents >>= print . f . transpose . map (map read . words) . lines
