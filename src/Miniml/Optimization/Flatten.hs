{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Miniml.Optimization.Flatten (gatherInfo, reduce) where

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

checkFlatten :: (Var, [Var], Cexp) -> State GatherState ()
checkFlatten (f, vl, body) = undefined

gatherInfo :: Cexp -> (Int, UsageInfo)
gatherInfo e0 =
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
      usage <- use #usageInfo
      #usageInfo % at' f % _Just % #_FunctionInfo % #arity %= update usage
      where
        update :: UsageInfo -> [Arity] -> [Arity]
        update usage = flip (loop usage) vl

        loop :: UsageInfo -> [Arity] -> [Value] -> [Arity]
        loop usage (a : as) (Var v : vs) =
          case (a, IM.lookup v usage) of
            (Bottom, Just (RecordInfo sz)) ->
              loop usage (Count sz False : as) (Var v : vs)
            (Bottom, _) -> Unknown : loop usage as vs
            (Unknown, Just (RecordInfo sz)) ->
              loop usage (Count sz True : as) (Var v : vs)
            (Unknown, _) -> Unknown : loop usage as vs
            (Count n _, Just (RecordInfo sz)) ->
              (if n == sz then a else Top) : loop usage as vs
            (Count n _, _) -> Count n True : loop usage as vs
            (Top, _) -> Top : loop usage as vs
        loop _ _ _ = []
    go (App _ vl) = traverse_ escape vl
    go (Fix l e) = do
      for_ l $ \(f, vl, _) -> do
        enter f (FunctionInfo (F (fmap (const Bottom) vl) Nothing False))
        traverse_ (`enter` ArgumentInfo (-1)) vl
      traverse_ (\(_, _, body) -> go body) l
      go e
      traverse_ checkFlatten l

reduce :: UsageInfo -> Cexp -> Cexp
reduce usage = cata go
  where
    go (AppF f@(Var fv) vl) = undefined
    go (FixF l e) = undefined
    go e = embed e