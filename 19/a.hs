import qualified Data.List.Split as SP
import Data.MemoTrie
import qualified Data.Set as S
import Data.List (inits, tails)
f ts = memoFix go
  where
    go go [] = 1
    go go flag = sum [go y | (x, y) <- tail (zip (inits flag) (tails flag)), x `S.member` ts]
g (ts:_:fls) = let ts' = S.fromList (SP.splitOn ", " ts)
                   r = map (f ts') fls
               in  (length (filter (> 0) r), sum r)
main = getContents >>= print . g . lines