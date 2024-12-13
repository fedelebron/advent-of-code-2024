import Data.MemoTrie

type Memo f = f -> f

f :: Memo ((Int, Int) -> Int)
f f (0, _) = 1
f f (i, 0) = f ((i - 1), 1)
f f (i, n) | r == 0 = f ((i - 1), (read a)) + f ((i - 1), (read b))
           | otherwise = f ((i - 1), (n * 2024))

  where
    s = show n
    m = length s
    (q, r) = quotRem m 2
    (a, b) = splitAt q s
      
main = getContents >>= print . sum . map (memoFix f . (75 ,) . read) . words
