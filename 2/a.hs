import Control.Monad (ap, liftM2)

g = (`ap` tail) . zipWith
h = foldr (&&) True
f :: [Integer] -> Bool
f xs = (h (g (<) xs) || h (g (>) xs)) && all (liftM2 (&&) (<= 3) (>= 1)) (g ((abs.).(-)) xs)
main = getContents >>= print . length . filter f . map (map read . words) . lines
