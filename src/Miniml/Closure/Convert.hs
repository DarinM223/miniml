module Miniml.Closure.Convert where

import Control.Monad.State.Strict (State)
import Data.Functor.Foldable (embed, project)
import Data.IntMap.Strict qualified as IM
import Data.IntSet ((\\))
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Tuple.Extra (fst3)
import Miniml.Closure.Free (Iteration (Iteration))
import Miniml.Cps (Cexp (..), Value (Label, Var), Var, shallowValues, var)
import Miniml.Shared (Access (Offp), fresh)
import Optics ((%~), (&))

convert :: IS.IntSet -> Iteration -> Cexp -> State Int Cexp
convert esc (Iteration v c _) = go IM.empty
  where
    rename sub x = x & var %~ \x' -> IM.findWithDefault x' x' sub
    noClosure = IS.fromList (IM.keys v) \\ c
    closureFreeVars fs = IS.unions ((v IM.!) <$> IS.elems fs) \\ fs \\ noClosure

    go :: IM.IntMap Var -> Cexp -> State Int Cexp
    go !sub (Fix fl@(_ : _) e) = do
      let closureRequired = IS.intersection c $ IS.fromList $ fst3 <$> fl
          freeVars = IS.toList $ closureFreeVars closureRequired
          escClos = IS.toList $ IS.intersection esc closureRequired
      escClos' <- traverse (const fresh) escClos
      let closureVars = fmap Label escClos' ++ fmap (rename sub . Var) freeVars
          closure = (,Offp 0) <$> closureVars
      e' <-
        if IS.null closureRequired
          then go sub e
          else do
            r <- maybe fresh (pure . fst) $ L.uncons escClos
            let sub' = IM.fromSet (const r) (closureRequired \\ esc) <> sub
            Record closure r . header escClos (Just 0) r <$> go sub' e
      fl' <- for fl $ \(f, vl, body) ->
        if IS.member f closureRequired
          then do
            let f' = fromMaybe f $ lookup f (zip escClos escClos')
                offset = L.elemIndex f escClos
            escClos'' <- traverse (const fresh) escClos
            freeVars' <- traverse (const fresh) freeVars
            let escClosSubs = IM.fromList $ zip escClos escClos''
                freeSubs = IM.fromList $ zip freeVars freeVars'
            f'' <- maybe fresh pure $ IM.lookup f escClosSubs
            let knownClosSubs = IM.fromSet (const f'') (closureRequired \\ esc)
                sub' = freeSubs <> escClosSubs <> knownClosSubs <> sub
            (f',f'' : vl,)
              . header escClos'' offset f''
              . unpack escClos'' offset f'' freeVars'
              <$> go sub' body
          else do
            let extraArgs = IS.toList $ v IM.! f \\ noClosure
            extraArgs' <- traverse (const fresh) extraArgs
            let sub' = IM.fromList (zip extraArgs extraArgs') <> sub
            (f,vl ++ extraArgs',) <$> go sub' body
      pure $ Fix fl' e'
    go !sub (App (Label f) vl) = go sub (App (Var f) vl)
    go !sub (App (Var f) vl) = rewriteCall sub f vl
    go !sub e = embed <$> traverse (go sub) (project e')
      where
        e' = e & shallowValues %~ rename sub

    rewriteCall sub f vl
      -- Call escaping closure.
      | IS.member f esc = callClos (rename sub (Var f)) (rename sub <$> vl)
      -- Call known closure.
      | Just record <- sub IM.!? f,
        IS.member f c =
          pure $ App (Label f) (rename sub <$> (Var record : vl))
      -- Call known function.
      | Just free <- v IM.!? f =
          let free' = IS.toList $ free \\ noClosure
           in pure $ App (Label f) (rename sub <$> (vl ++ fmap Var free'))
      -- Call continuation escaping closure.
      | otherwise = callClos (rename sub (Var f)) (rename sub <$> vl)
    callClos f vl = (\w -> Select 0 f w (App (Var w) (f : vl))) <$> fresh

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
