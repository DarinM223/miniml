module Miniml.Closure.Convert where

import Control.Monad.State.Strict (State)
import Data.Functor.Foldable (embed, project)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Tuple.Extra (fst3)
import Miniml.Closure.Free (Iteration (Iteration))
import Miniml.Cps (Cexp (..), Value (Label, Var), Var, shallowValues)
import Miniml.Shared (Access (Offp), fresh)
import Optics (gplate, (%~), (&))

convert :: IS.IntSet -> Iteration -> Cexp -> State Int Cexp
convert esc (Iteration v c) = go IM.empty
  where
    noClosure = IS.fromList (IM.keys v) IS.\\ c

    rename :: IM.IntMap Var -> Value -> Value
    rename sub (Var x) | Just x' <- sub IM.!? x = Var x'
    rename sub (Label l) | Just l' <- sub IM.!? l = Label l'
    rename _ x = x

    go :: IM.IntMap Var -> Cexp -> State Int Cexp
    go sub (Fix fl@(_ : _) e) = do
      let fs = fst3 <$> fl
          closureRequired = filter (`IS.member` c) fs
          freeVars =
            IS.toList $
              IS.unions ((v IM.!) <$> closureRequired)
                IS.\\ IS.fromList fs
                IS.\\ noClosure
          escClos = filter (`IS.member` esc) closureRequired
      escClos' <- traverse (const fresh) escClos
      let closure = [(rename sub (Var x), Offp 0) | x <- escClos' ++ freeVars]
      e' <-
        if null closureRequired
          then go sub e
          else do
            r <- maybe fresh (pure . fst) $ L.uncons escClos
            let sub' = IM.fromList [(f, r) | f <- filter (not . (`IS.member` esc)) closureRequired] <> sub
            Record closure r . header escClos (Just 0) r <$> go sub' e
      fl' <- for fl $ \(f, vl, body) ->
        if f `elem` closureRequired
          then do
            let f' = fromMaybe f $ lookup f (zip escClos escClos')
                offset = L.elemIndex f escClos
            escClos'' <- traverse (const fresh) escClos
            f'' <- maybe fresh pure $ lookup f (zip escClos escClos'')
            freeVars' <- traverse (const fresh) freeVars
            let sub' = IM.fromList (zip escClos escClos'' ++ zip freeVars freeVars') <> sub
            body'' <-
              header escClos'' offset f''
                . unpack escClos'' offset f'' freeVars'
                <$> go sub' body
            pure (f', f'' : vl, body'')
          else do
            let extraArgs = IS.toList $ v IM.! f
            extraArgs' <- traverse (const fresh) extraArgs
            let sub' = IM.fromList (zip extraArgs extraArgs') <> sub
            (f,vl ++ extraArgs',) <$> go sub' body
      pure $ Fix fl' e'
    go sub (App (Label f) vl) = go sub (App (Var f) vl)
    go sub (App (Var f) vl) =
      fmap (gplate @Value %~ rename sub) (rewriteCall sub f vl)
    go sub e = embed <$> traverse (go sub) (project e')
      where
        e' = e & shallowValues %~ rename sub

    rewriteCall sub f vl
      -- Call escaping closure.
      | IS.member f esc = callClos f vl
      -- Call known closure.
      | Just record <- sub IM.!? f,
        IS.member f c =
          pure $ App (Var f) (Var record : vl)
      -- Call known function.
      | Just free <- v IM.!? f =
          pure $ App (Var f) (vl ++ fmap Var (IS.toList free))
      -- Call continuation escaping closure.
      | otherwise = callClos f vl

callClos :: Var -> [Value] -> State Int Cexp
callClos f vl = (\w -> Select 0 (Var f) w (App (Var w) (Var f : vl))) <$> fresh

header :: [Var] -> Maybe Int -> Var -> Cexp -> Cexp
header fs (Just offset) f' e =
  foldr (\(i, f) -> Offset i (Var f') f) e $
    filter ((/= 0) . fst) (zip [-offset ..] fs)
header fs Nothing f' e =
  foldr (\(i, f) -> Offset i (Var f') f) e (zip [0 ..] fs)

unpack :: [Var] -> Maybe Int -> Var -> [Var] -> Cexp -> Cexp
unpack fs offset f' freeVars e =
  foldr (\(i, v) -> Select i (Var f') v) e $ zip [start ..] freeVars
  where
    start = maybe id (flip (-)) offset (length fs)