import Data.Bits
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed ((!))
mask = ((2^24) - 1)
m1 = (19*19*19)
m2 = (m1*19)

doRound b = let b1 = (b `xor` (b `shiftL` 6)) .&. mask
                b2 = (b1 `xor` (b1 `shiftR` 5)) .&. mask
                b3 = (b2 `xor` (b2 `shiftL` 11)) .&. mask
            in b3
solve xs = runST $ do
  dp <- MV.replicate (m2 * n) (-10 :: Int)
  su <- emu 0 xs 0 dp
  dp' <- V.freeze dp
  return (su, maximum (map (s2 dp') [0 .. m2 - 1]))
  where
    n = length xs
    s2 dp j = let t = j * n
                  f i = if dp ! (t + i) /= -10 then dp ! (t + i) else 0
                  r = sum $ map f [0 .. n - 1]
              in r
    emu _ [] su dp = return su
    emu i (a:as) su dp = let (ix, b) = foldr (const (uncurry r1)) (0, a) [0 .. 3]
                         in do
                           MV.write dp (ix * n + i) (b `mod` 10)
                           (_, b', _) <- foldM (const . r2 i) (ix, b, dp) [4 .. 2000 - 1]
                           emu (i + 1) as (su + b') dp
    r1 ix a = let b = doRound a
                  ix' = ix * 19 + 9 + (b `mod` 10) - (a `mod` 10)
              in  (ix', b)
    r2 i (ix, a, dp) = let b = doRound a
                           ix' = ((ix `mod` m1) * 19) + 9 + (b `mod` 10) - (a `mod` 10)
                           j = ix' * n + i
                       in do
                         z <- MV.read dp j
                         when (z == -10) $ do
                          (MV.write dp j (b `mod` 10))
                         return (ix', b, dp)
main = getContents >>= print . solve . map read . lines