import Data.Char
f _ k [] = k
f m k ('m':'u':'l':'(':xs) =
  let (a, b) = span isDigit xs
      l = length a
  in  if l == 0 || l > 3 then f m k xs
      else case b of
           (',':ys) -> let (c, d) = span isDigit ys
                           n = length c
                       in  if n == 0 || n > 3 then f m k ys
                           else case d of
                             (')':zs) -> f m (k + m * (read a) * (read c)) zs
                             otherwise -> f m k d 
           otherwise -> f m k xs
f m k ('d':'o':'n':'\'':'t':'(':')':xs) = f 0 k xs
f m k ('d':'o':'(':')':xs) = f 1 k xs
f m k (r:rs) = f m k rs
        
main = getContents >>= print . f 1 0
