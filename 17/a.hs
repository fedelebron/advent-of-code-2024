{-# LANGUAGE LambdaCase #-}
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Bits (xor)
emu ins rip = go rip
  where
    go rip _ _ _ | rip >= V.length ins = []
    go rip a b c = case (ins ! rip, ins ! (rip + 1)) of
     (0, x) -> go (rip + 2) (a `div` (2^(combo x))) b c
     (1, x) -> go (rip + 2) a (b `xor` x) c
     (2, x) -> go (rip + 2) a ((combo x) `mod` 8) c
     (3, x) -> go (if a == 0 then rip + 2 else x) a b c 
     (4, _) -> go (rip + 2) a (b `xor` c) c
     (5, x) -> (combo x `mod` 8):(go (rip + 2) a b c)
     (6, x) -> go (rip + 2) a (a `div` (2^(combo x))) c
     (7, x) -> go (rip + 2) a b (a `div` (2^(combo x)))
     where 
      combo = \case
        x | 0 <= x && x <= 3 -> x
        4 -> a
        5 -> b
        6 -> c
f [pa, pb, pc, _, prog] = let l = length "Register A: "
                              a :: Int = read (drop l pa)
                              b :: Int = read (drop l pb)
                              c :: Int = read (drop l pc)
                              ins :: V.Vector Int = V.fromList . read . ('[':) . (++"]") . drop (length "Program: ") $ prog
                          in emu ins 0 a b c
main = getContents >>= print . f . lines