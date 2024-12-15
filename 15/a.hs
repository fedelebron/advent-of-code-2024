import Data.Matrix
import Control.Arrow
instance Num (Int, Int) where
  (a, b) + (c, d) = (a + c, b + d)
s c x = let (a, _:b) = span (/= c) x
        in (a, b)
fi x m = case [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m], m ! (i, j) == x]
         of y:_ -> y
toDelta c = case c of
  '>' -> (0, 1)
  '<' -> (0, -1)
  '^' -> (-1, 0)
  'v' -> (1, 0)
ss = unsafeSet
tm ep d m = case y of
  '#' -> Nothing
  '.' -> return (ss '.' ep (ss x nep m))
  _ -> do
    m' <- case (y, fst d == 0) of
            ('[', False) -> tm (nep + (0, 1)) d m
            (']', False) -> tm (nep + (0, -1)) d m
            _ -> return m
    m'' <- tm nep d m'
    return (ss '.' ep (ss x nep m''))
  where
    nep = ep + d
    x = m ! ep
    y = m ! nep
ev d (m, p) = maybe (m, p) (, p + d) (tm p d m)
score (i, j) c | c `elem` "O[" = 100*(i - 1) + (j - 1)
score _ _ = 0
f x moves = let mat = fromLists x
                pos = fi '@' mat
                (m, _) = foldr ev (mat, pos) (reverse (map toDelta moves))
            in sum (mapPos score m)
g = f . map (p2 =<<)
  where
    p2 '.' = ".."
    p2 '#' = "##"
    p2 '@' = "@."
    p2 'O' = "[]"
main = getContents >>= print . (uncurry f &&& uncurry g) . (id *** concat) . s "" . lines