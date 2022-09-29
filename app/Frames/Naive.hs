module Frames.Naive where

import Regex
import Frames
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

type Slots x v = [(x, v)]
type Links l   = [(l, Frame)]

data HF l x v
  = HF { slots :: Slots x v
       , links :: Links l }

type Heap l x v = [HF l x v]

type M l x v = ExceptT String (ReaderT Frame (State (Heap l x v)))

run :: M l x v a -> Either String a
run m = fst $ flip runState [HF [] []] $ flip runReaderT 0 $ runExceptT m

class ResolutionPolicy l x where
  shortestPath :: x -> [(v, [l])] -> Maybe (v, [l])

instance ( Eq x
         , Eq l
         , ResolutionPolicy l x )
      => Frameful l x v (M l x v) where
  look x r = do
    mps <- look' x r
    return
      $ shortestPath x
      $ foldr (\ (mv, p) mps' -> case mv of Just v -> (v, p):mps'; _ -> mps') [] mps
    where
      look' x r = do
        f <- ask
        h <- get
        let hf = h!!f
            continue = foldM
                         (\ mps l -> case lookup l (links hf) of
                                       Just f' -> do
                                         mps <- local (const f') (look' x r)
                                         return $ map (\ (mv, p) -> (mv, l:p)) mps
                                       Nothing -> return $ mps)
                         []
                         (frontier r)
        if isEmpty r
          then case lookup x (slots hf) of
                 Just v  -> return $ [(Just v, [])]
                 Nothing -> continue
          else continue

  initf slots links = do
    h <- get
    put (h ++ [HF slots links])
    return $ length h

  curf = ask

  withf f = local (const f)

  err = throwError
