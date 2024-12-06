import Control.Arrow ((***))
import qualified Data.Map as M
import Data.List
s :: Eq a => a -> [a] -> ([a], [a])
s = ((id *** tail) .) . span . (/=)
m v = v !! (div (length v) 2)
v m (x:xs) = all (x `notElem`) (map (flip (M.findWithDefault []) m) xs) && v m xs
v _ [] = True
t d u | r:_ <- u \\ (concat (M.elems d)) = r : t (M.delete r d) (u \\ [r])
t d [] = []
f d = print . foldl (uncurry go) (0, 0)
  where
    go sa sb u | v d u = (sa + m u, sb)
               | otherwise = (sa, sb + m (t (M.filterWithKey (const . (`elem` u)) d) u))
main = getContents >>= uncurry f . (foldr (uncurry (M.insertWith (++))) M.empty . map ((read *** return . read) . s '|') *** (map (read . (++"]") . ('[':)))) . s "" . lines
