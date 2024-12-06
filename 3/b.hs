import Data.Char
r xs | (a, b) <- span isDigit xs,
       l <- length a,
       l >= 1 && l <= 3 = Just (read a, b)
     | otherwise = Nothing
     
f _ k [] = k
f m k ('m':'u':'l':'(':xs)
  | Just (ra, (',':ys)) <- r xs,
    Just (rc, (')':zs)) <- r ys = f m (k + m * ra * rc) zs
f m k ('d':'o':'n':'\'':'t':'(':')':xs) = f 0 k xs
f m k ('d':'o':'(':')':xs) = f 1 k xs
f m k (r:rs) = f m k rs
        
main = getContents >>= print . f 1 0
