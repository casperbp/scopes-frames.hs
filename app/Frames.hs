module Frames where

import Regex

type Frame = Int

class Monad m => Frameful l x v m | m -> x, m -> v, m -> l where
  look  :: x -> RE l -> m (Maybe (v, [l]))
  initf :: [(x,v)] -> [(l,Frame)] -> m Frame
  curf  :: m Frame
  withf :: Frame -> m a -> m a
  err   :: String -> m a

