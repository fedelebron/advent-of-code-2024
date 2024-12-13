import Text.Parsec
import Control.Arrow
s _ [] = []
s c l = let (a, b) = span (/= c) l
        in case b of
          [] -> [a]
          _:bs -> a:(s c bs)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
num = read <$> many1 digit
snum = sign <*> num
p = (,) <$> (string "Button " *> anyChar *> string ": X" *> snum) <*> (string ", Y" *> snum)
r = (,) <$> (string "Prize: X=" *> snum) <*> (string ", Y=" *> snum)
solve k [la, lb, lc] = either (const 0) id (f k <$> parse p "" la <*> parse p "" lb <*> parse r "" lc)
f k (ax, ay) (bx, by) (x, y) = g ax ay bx by (x + k) (y + k)
g ax ay bx by x y
  | ax * ay * bx * by == 0 = 0
  | (b, 0) <- (ay * x - ax * y) `divMod` (ay * bx - ax * by),
    (a, 0) <- (x - bx * b) `divMod` ax = 3*a + b
  | otherwise = 0
main = getContents >>= print . (sum *** sum) . unzip . map (solve 0 &&& solve 10000000000000) . s "" . lines