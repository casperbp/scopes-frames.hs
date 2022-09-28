module Regex where

import Data.List

data RE l
  = Empty
  | Stuck
  | Elem l
  | Pipe (RE l) (RE l)
  | Dot (RE l) (RE l)
  | Star (RE l)


frontier :: Eq l => RE l -> [l]
frontier Empty = []
frontier Stuck = []
frontier (Elem l) = [l]
frontier (Pipe r1 r2) = nub $ frontier r1 ++ frontier r2
frontier (Dot r1 _) = frontier r1
frontier (Star r) = frontier r

isEmpty :: RE l -> Bool
isEmpty Empty = True
isEmpty Stuck = False
isEmpty (Elem _) = False
isEmpty (Pipe r1 r2) = isEmpty r1 || isEmpty r2
isEmpty (Dot r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Star _) = True

derive :: Eq l => l -> RE l -> RE l
derive _ Empty = Stuck
derive _ Stuck = Stuck
derive l (Elem l') | l == l' = Empty
                   | otherwise = Stuck
derive l (Pipe r1 r2) = case (derive l r1, derive l r2) of
  (Stuck, r2') -> r2'
  (r1', Stuck) -> r1'
  (r1', r2')   -> Pipe r1' r2'
derive l (Dot r1 r2) = if isEmpty r1
  then derive l r2
  else Dot (derive l r1) r2
derive l (Star r) = Dot (derive l r) (Star r)
