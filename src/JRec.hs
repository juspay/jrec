{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module JRec
  ( unField,
    union,
    (:=) (..),
    Rec,
    pattern Rec,
  )
where

import Control.Lens ((&), (.~), (^.), coerced)
import qualified Data.Generics.Product.Fields as GL
import qualified Data.Generics.Wrapped as GL
import Data.Proxy
import GHC.Exts (Any)
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import Generic.Data
import JRec.Field
import qualified JRec.Super as R
import JRec.Super ((:=) (..), Rec)
import JRec.Tuple
import Unsafe.Coerce

----------------------------------------------------------------------------
-- unField
----------------------------------------------------------------------------

unField :: field ~ field' => R.FldProxy field -> (field' R.:= value) -> value
unField _ (_ R.:= value) = value

----------------------------------------------------------------------------
-- Other operations
----------------------------------------------------------------------------

union ::
  forall lhs rhs res.
  ( KnownNat (R.RecSize lhs),
    KnownNat (R.RecSize rhs),
    KnownNat (R.RecSize lhs + R.RecSize rhs),
    res ~ R.RecAppend lhs rhs,
    R.RecCopy lhs lhs res,
    R.RecCopy rhs rhs res
  ) =>
  Rec lhs ->
  Rec rhs ->
  Rec res
union = R.combine

----------------------------------------------------------------------------
-- Generic
----------------------------------------------------------------------------

type Sel name value = S1 ('MetaSel ('Just name) 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedStrict) (Rec0 value)

type family Sels fields where
  Sels '[] = U1
  Sels '[name R.:= value] = Sel name value
  Sels ((name R.:= value) ': xs) = Sel name value :*: Sels xs

type RecordRep fields =
  D1
    ('MetaData "Record" "Rec" "" 'False)
    ( C1
        ('MetaCons "Record" 'PrefixI 'True)
        (Sels fields)
    )

instance
  (R.FromNative (RecordRep fields) fields, R.ToNative (RecordRep fields) fields) =>
  Generic (Rec fields)
  where
  type
    Rep (Rec fields) =
      RecordRep fields
  from r = R.toNative' r
  to rep = R.fromNative' rep

----------------------------------------------------------------------------
-- generic-lens
----------------------------------------------------------------------------

instance {-# OVERLAPPING #-} (R.Set field fields a' ~ fields', R.Set field fields' a ~ fields, R.Has field fields a, R.Has field fields' a') => GL.HasField field (Rec fields) (Rec fields') a a' where
  field = R.lens (R.FldProxy @field)

instance {-# OVERLAPPING #-} (R.Set field fields a ~ fields, R.Has field fields a) => GL.HasField' field (Rec fields) a where
  field' = R.lens (R.FldProxy @field)

pattern Rec ::
  ( RecTuple tuple fields
  ) =>
  tuple ->
  Rec fields
pattern Rec a <-
  (toTuple -> a)
  where
    Rec = fromTuple


