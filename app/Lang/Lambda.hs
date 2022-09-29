module Lang.Lambda where

import Regex
import Frames
import Frames.Naive

data Expr
  = Lit Int
  | Lam String Expr
  | App Expr Expr
  | Var String
  deriving ( Show
           , Read )

data Val
  = Clo String Expr Frame
  | Num Int
  deriving ( Show
           , Read )

data Label = P
  deriving Eq

instance ResolutionPolicy Label String where
  shortestPath _ ps = if null ps
    then Nothing
    else Just $ go (head ps) (tail ps)
    where
      go p [] = p
      go p (p':ps) = if length p <= length p' then go p ps else go p' ps

interp :: Frameful Label String Val m => Expr -> m Val
interp (Lit i)     = return $ Num i
interp (Lam x e)   = do f <- curf; return $ Clo x e f
interp (App e1 e2) = do
  v1 <- interp e1
  v2 <- interp e2
  case v1 of
    Clo x e f -> do
      f' <- initf [(x, v2)] [(P, f)]
      withf f' (interp e)
    _ -> err "Bad application!"
interp (Var x) = do
  res <- look x (Star (Elem P))
  case res of
    Just (v, _) -> return v
    Nothing -> err $ "Bad lookup of " ++ x


test0 :: Expr
test0 =
  App (App (Lam "x" (Lam "y" (Var "y"))) (Lit 0)) (Lit 1)

