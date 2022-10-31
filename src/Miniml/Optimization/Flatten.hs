{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Flatten
  ( fieldExistsAllBranches,
    gatherInfo,
    reduce,
  )
where

import Control.Monad (replicateM, when)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.State.Strict (State, execState, state)
import Data.Foldable (foldlM, for_, traverse_)
import Data.Functor.Foldable (embed, project)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), Value (..), Var)
import Miniml.Shared (Access (Offp, Selp), fresh)
import Optics (at', use, zoom, (%), _Just)
import Optics.State.Operators ((%=), (.=), (?=))

-- | The arity information of a function parameter.
data Arity
  = -- | Initial value, needs to be updated.
    Bottom
  | -- | Isn't a known record yet, but it can still be updated.
    Unknown
  | -- | Is a known record.
    Count
      {-# UNPACK #-} !Int
      -- ^ Number of fields in the record.
      !Bool
      -- ^ True if at some function calls this parameter was unknown.
      -- False if at all function calls this parameter was a known record.
  | -- | Cannot flatten this argument. Cannot be updated.
    Top
  deriving (Show)

data Info
  = FunctionInfo !FunctionInfo
  | -- | Highest field number selected
    ArgumentInfo {-# UNPACK #-} !Int
  | -- | Number of fields
    RecordInfo {-# UNPACK #-} !Int
  deriving (Show, Generic)

data FunctionInfo = F
  { arity :: [Arity],
    alias :: Maybe Var,
    escape :: !Bool
  }
  deriving (Show, Generic)

type UsageInfo = IM.IntMap Info

data GatherState = GatherState
  { usageInfo :: !UsageInfo,
    counter :: {-# UNPACK #-} !Int,
    clicks :: {-# UNPACK #-} !Int
  }
  deriving (Generic)

enter :: Int -> Info -> State GatherState ()
enter k v = #usageInfo % at' k ?= v

select :: Value -> Int -> State GatherState ()
select (Var v) i = #usageInfo % at' v % _Just % #_ArgumentInfo %= max i
select (Label v) i = select (Var v) i
select _ _ = pure ()

escape :: Value -> State GatherState ()
escape (Var v) = #usageInfo % at' v % _Just % #_FunctionInfo % #escape .= True
escape (Label v) = escape (Var v)
escape _ = pure ()

field :: Value -> Access -> State GatherState ()
field v (Selp i _) = select v i
field v _ = escape v

fieldExistsAllBranches :: Var -> Int -> Cexp -> Bool
fieldExistsAllBranches v j = go
  where
    go (Record fields _ e) = any goField fields || go e
    go (Select i (Var v') _ _) | v == v' && i == j = True
    go (Fix _ e) = go e
    go e = if length e' > 1 then all go e' else any go e'
      where
        e' = project e

    goField (Var v', Selp i _) | v == v' && i == j = True
    goField _ = False

checkFlatten :: Int -> (Var, [Var], Cexp) -> State GatherState ()
checkFlatten maxRegs (f, vl, body) = do
  usageInfo <- use #usageInfo
  case IM.lookup f usageInfo of
    Just (FunctionInfo (F arity _ esc)) -> do
      let go (a : as) (v : vs) !headroom =
            case (a, IM.lookup v usageInfo) of
              (Count c someNonRecord, Just (ArgumentInfo j))
                | j > -1
                    && headroom' > 0
                    && ( not (someNonRecord || esc)
                           || (j == c - 1 && fieldExistsAllBranches v j body)
                       ) ->
                    a : go as vs headroom'
                where
                  headroom' = headroom - (c - 1)
              _ -> Top : go as vs headroom
          go _ _ _ = []
          arity' = go arity vl (maxRegs - 1 - length arity)
      let fnInfo = #usageInfo % at' f % _Just % #_FunctionInfo
      fnInfo % #arity .= arity'
      when (any flattened arity') $ do
        f' <- zoom #counter fresh
        fnInfo % #alias ?= f'
        #clicks %= (+ 1)
    _ -> pure ()
  where
    flattened (Count _ _) = True
    flattened _ = False

gatherInfo :: Int -> Cexp -> State Int (Int, UsageInfo)
gatherInfo maxRegs e0 = state $ \(!tmp) ->
  let GatherState !i !tmp' !clk = execState (go e0) (GatherState IM.empty tmp 0)
   in ((clk, i), tmp')
  where
    go (Record vl w e) =
      enter w (RecordInfo (length vl)) >> traverse_ (uncurry field) vl >> go e
    go (Select i v _ e) = select v i >> go e
    go (Offset _ v _ e) = escape v >> go e
    go (Switch v el) = escape v >> traverse_ go el
    go (Primop _ vs _ el) = traverse_ escape vs >> traverse_ go el
    go (App (Var f) vl) = do
      traverse_ escape vl
      usageInfo <- use #usageInfo
      #usageInfo % at' f % _Just % #_FunctionInfo % #arity %= update usageInfo
      where
        update :: UsageInfo -> [Arity] -> [Arity]
        update usageInfo as = uncurry (updateParam usageInfo) <$> zip as vl

        updateParam :: UsageInfo -> Arity -> Value -> Arity
        updateParam usageInfo arity (Var v) =
          case (arity, IM.lookup v usageInfo) of
            (Bottom, Just (RecordInfo sz)) -> Count sz False
            (Bottom, _) -> Unknown
            (Unknown, Just (RecordInfo sz)) -> Count sz True
            (Unknown, _) -> Unknown
            (Count n _, Just (RecordInfo sz)) -> if n == sz then arity else Top
            (Count n _, _) -> Count n True
            (Top, _) -> Top
        updateParam _ _ _ = Top
    go (App _ vl) = traverse_ escape vl
    go (Fix l e) = do
      for_ l $ \(f, vl, _) -> do
        enter f (FunctionInfo (F (Bottom <$ vl) Nothing False))
        traverse_ (`enter` ArgumentInfo (-1)) vl
      traverse_ (\(_, _, body) -> go body) l
      go e
      traverse_ (checkFlatten maxRegs) l

reduce :: UsageInfo -> Cexp -> State Int Cexp
reduce usage = go
  where
    go (App (Var f) vs)
      | Just (FunctionInfo (F as (Just f') _)) <- IM.lookup f usage =
          runContT
            (foldlM collectArgs [] (zip as vs))
            (pure . App (Var f') . reverse)
      where
        collectArgs args (Count cnt _, v) =
          foldlM (unpackField v) args [0 .. cnt - 1]
        collectArgs args (_, v) = pure $ v : args

        unpackField v args i = ContT $ \c -> do
          z <- fresh
          Select i v z <$> c (Var z : args)
    go (Fix l e) = do
      l' <- processArgs l
      embed <$> traverse go (project (Fix l' e))
      where
        processArgs :: [(Var, [Var], Cexp)] -> State Int [(Var, [Var], Cexp)]
        processArgs [] = pure []
        processArgs ((f, vs, body) : rest)
          | Just (FunctionInfo (F as (Just f') _)) <- IM.lookup f usage = do
              (vf, body') <- foldlM newArgs (id, body) $ zip as vs
              ws <- traverse (const fresh) vs
              -- Adds the function with flattened arguments, `f'`, and rewrites
              -- `f` to call `f'` so that it can still work if `f` escapes.
              -- It looks like `f` is calling itself instead of `f'`, but the
              -- body of `f` hasn't been reduced yet. After being reduced, it
              -- will be rewritten to call `f'` with the flattened arguments.
              let fdef = (f, ws, App (Var f) (Var <$> ws))
                  fdef' = (f', vf [], body')
              (fdef :) . (fdef' :) <$> processArgs rest
        processArgs (fdef : rest) = (fdef :) <$> processArgs rest

        newArgs (vf, body) (Count j _, v) = do
          new <- replicateM j fresh
          -- Pack the flattened arguments back into a record inside the body.
          -- The contraction phase will remove unnecessary packing and selecting.
          pure (vf . (new ++), Record ((,Offp 0) . Var <$> new) v body)
        newArgs (vf, body) (_, v) = pure (vf . (v :), body)
    go e = embed <$> traverse go (project e)