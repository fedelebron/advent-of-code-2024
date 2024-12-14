import Text.Parsec
import Control.Arrow
import Data.Either
import Data.Monoid
import Data.Matrix hiding ((<|>))
import Control.Monad
sign = (char '-' >> return negate) <|> (return id)
num = read <$> many1 digit
snum = sign <*> num
p = (,) <$> (flip (,) <$> (string "p=" *> num) <*> (char ',' *> num))
        <*> (flip (,) <$> (string " v=" *> snum) <*> (char ',' *> snum))
w = 101
h = 103
quad = (\(Sum a, Sum b, Sum c, Sum d) -> a*b*c*d) . mconcat . map (g . fst)
  where
    w2 = w `div` 2
    h2 = h `div` 2
    x = Sum . fromEnum
    g (i, j) = (x (i < h2 && j < w2), x (i < h2 && j > w2), x (i > h2 && j < w2), x (i > h2 && j > w2))
g ((i, j), (di, dj)) = (((i + di) `mod` h, (j + dj) `mod` w), (di, dj))
f = (quad .) . map . flip (foldr (const g)) . enumFromTo 1
d xs = let m' = foldr u (zero h w) (map fst xs)
       in  print m' >> return xs
  where
    u (i, j) m = unsafeSet (1 + unsafeGet (i + 1) (j + 1) m) (i + 1, j + 1)  m
f' yy s = foldM r s (enumFromTo 1 yy) >> return ()
  where
    r x i = print i >> d (map g x)
main = getContents >>= (uncurry (>>)) . (print *** id) . (f 100 &&& f' 10000) . fromRight [] . sequence . map (parse p "") . lines