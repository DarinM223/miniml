{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Flatten (gatherInfo, reduce) where

import Control.Monad (when)
import Control.Monad.State.Strict (State, execState)
import Data.Foldable (for_, traverse_)
import Data.Functor.Foldable (cata, embed)
import Data.IntMap qualified as IM
import GHC.Generics (Generic)
import Miniml.Cps (Cexp (..), CexpF (..), Value (..), Var)
import Miniml.Shared (Access (Selp))
import Optics (at', use, (%), _Just)
import Optics.State.Operators ((%=), (.=), (?=))

-- | The arity information of a function parameter.
data Arity
  = -- | Initial value, needs to be updated.
    Bottom
  | -- | Isn't a known record yet, but it can still be updated.
    Unknown
  | -- | Is a known record.
    Count
      { numFields :: {-# UNPACK #-} !Int,
        -- | True if at some function calls this parameter was unknown.
        -- False if at all function calls this parameter was a known record.
        anyUnknownCalls :: !Bool
      }
  | -- | Cannot flatten this argument. Cannot be updated.
    Top

data Info
  = FunctionInfo !FunctionInfo
  | -- | Highest field number selected
    ArgumentInfo {-# UNPACK #-} !Int
  | -- | Number of fields
    RecordInfo {-# UNPACK #-} !Int
  deriving (Generic)

data FunctionInfo = F
  { arity :: [Arity],
    alias :: Maybe Var,
    escape :: !Bool
  }
  deriving (Generic)

type UsageInfo = IM.IntMap Info

data GatherState = GatherState
  { usageInfo :: {-# UNPACK #-} !UsageInfo,
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

click :: State GatherState ()
click = #clicks %= (+ 1)

checkFlatten :: Int -> (Var, [Var], Cexp) -> State GatherState ()
checkFlatten maxRegs (f, vl, _) = do
  usageInfo <- use #usageInfo
  case IM.lookup f usageInfo of
    Just (FunctionInfo (F arity _ esc)) -> do
      let go (a : as) (v : vs) !headroom =
            case (a, IM.lookup v usageInfo) of
              (Count c someNonRecord, Just (ArgumentInfo j))
                | j > -1 && headroom' > 0 && not (someNonRecord || esc) ->
                    a : go as vs headroom'
                where
                  headroom' = headroom - (c - 1)
              _ -> Top : go as vs headroom
          go _ _ _ = []
          arity' = go arity vl (maxRegs - 1 - length arity)
      let fnInfo = #usageInfo % at' f % _Just % #_FunctionInfo
      fnInfo % #arity .= arity'
      when (any flattened arity') $ fnInfo % #alias ?= f >> click
    _ -> pure ()
  where
    flattened (Count _ _) = True
    flattened _ = False

gatherInfo :: Int -> Cexp -> (Int, UsageInfo)
gatherInfo maxRegs e0 =
  let GatherState !i !c = execState (go e0) (GatherState IM.empty 0)
   in (c, i)
  where
    go :: Cexp -> State GatherState ()
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

reduce :: UsageInfo -> Cexp -> Cexp
reduce usage = cata go
  where
    go (AppF f@(Var fv) vl) = undefined
    go (FixF l e) = undefined
    go e = embed e