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
dijkstra m s e = let is = (PQ.singleton s 0, S.empty, M.singleton s 0, M.empty, Nothing)
                     (_, _, dist, preds, best) = execState go is
                     es = [r | d <- dirs, let r = (e, d), M.lookup r dist == best]
                 in  (best, es, preds)
  where
    go = do
      pq <- use _1
      case PQ.minView pq of
        Nothing -> return ()
        Just (v :-> d, pq') -> do
          best <- use _5
          case best of
            Just k | k < d -> return ()
            _ -> do
              when (fst v == e) $ _5 %= maybe (Just d) (Just . min d)
              pq <- (_1 <.= pq')
              seen <- (_2 <%= S.insert v)
              forM (nes m v) $ \(d', w) -> do
                let nd = d + d'
                when (w `S.notMember` seen) $ do
                  case PQ.lookup w pq of
                    Nothing -> do
                      _1 %= PQ.insert w nd
                      _3 %= M.insert w nd
                      _4 %= M.insert w [v]
                    Just ed -> do
                      if ed == nd then _4 %= M.adjust (v:) w
                      else if ed > nd then do
                        _4 %= M.adjust (const [v]) w
                        _3 %= M.adjust (const nd) w
                        _1 %= PQ.adjust (const nd) w
                        else return ()
              go
visi pre = go S.empty
  where
    go s [] = s
    go s (v@(pv, _):vs) = go (S.insert pv s) (vs ++ M.findWithDefault [] v pre)
solve m = let (pos, end) = (fi 'S' m, fi 'E' m)
              (d, es, pre) = dijkstra m (pos, (0, 1)) end 
          in (d, length (visi pre es))
main = getContents >>= print . solve . fromLists . lines 