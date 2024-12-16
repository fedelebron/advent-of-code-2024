import Data.Matrix
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding((:->)))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Lazy
import Control.Monad
import Control.Lens
instance Num (Int, Int) where
  (a, b) + (c, d) = (a + c, b + d)
fi x m = case [(i, j) | i <- [1 .. nrows m], j <- [1 .. ncols m], m ! (i, j) == x]
         of y:_ -> y
dirs = [(0, 1), (0, -1), (-1, 0), (1, 0)]
nes m (v, di) = let nn = [(1000, (v, d)) | d <- dirs, d /= di]
                in  if m ! (v + di) == '#'
                    then nn
                    else (1, (v + di, di)):nn
dijkstra m s e = let is = (PQ.singleton s 0, S.empty, M.singleton s 0, M.empty)
                     (_, _, dist, preds) = execState go is
                     es = [(r, k) | d <- dirs, let r = (e, d), Just k <- [M.lookup r dist]]
                     md = minimum (map snd es)
                     res = map fst $ filter ((== md) . snd) es
                 in  (md, res, preds)
  where
    go = do
      pq <- use _1
      case PQ.minView pq of
        Nothing -> return ()
        Just (v :-> d, pq') -> do
          pq <- (_1 <.= pq')
          seen <- (_2 <%= S.insert v)
          forM_ (nes m v) $ \(d', w) -> do
            let nd = d + d'
            when (w `S.notMember` seen) $ do
              case PQ.lookup w pq of
                Nothing -> do
                  _1 %= PQ.insert w nd
                  _3 %= M.insert w nd
                  _4 %= M.insert w [v]
                Just ed -> do
                  if ed == nd then _4 %= (M.adjust (v:) w)
                  else if ed > nd then do
                    _4 %= (M.adjust (const [v]) w)
                    _3 %= (M.adjust (const nd) w)
                    _1 %= PQ.adjust (const nd) w
                    else return ()
          go
visi pre es = go es S.empty
  where
    go [] s = s
    go (v@(pv, _):vs) s = go (vs ++ M.findWithDefault [] v pre) (S.insert pv s)
solve m = let (pos, end) = (fi 'S' m, fi 'E' m)
              (d, es, pre) = dijkstra m (pos, (0, 1)) end 
          in (d, length (visi pre es))
main = getContents >>= print . solve . fromLists . lines 