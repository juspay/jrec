{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module JRec
  ( Record,
    pattern Record,
    (:=:),
    pattern (:=:),
    pattern ExactRecord,
    unField,
    union,
  )
where

import Control.Lens ((.~), coerced)
import qualified Data.Generics.Product.Fields as GL
import qualified Data.Generics.Wrapped as GL
import Data.Proxy
import GHC.Exts (Any)
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import Generic.Data
import qualified JRec.Super as R
import Control.Lens ((&), (^.))
import JRec.Super ((:=))
import JRec.Tuple
import JRec.Field
import Unsafe.Coerce

----------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------

newtype Record fields = MkRecord (R.Rec fields)

deriving instance R.RecEq fields fields => Eq (Record fields)

instance
  (Generic (Record fields), GShow0 (Rep (Record fields))) =>
  Show (Record fields)
  where
  show = (\x -> if null x then "{}" else x) . unwords . drop 1 . words . flip (gshowsPrec 0) ""

----------------------------------------------------------------------------
-- unField
----------------------------------------------------------------------------

unField :: field ~ field' => R.FldProxy field -> (field' R.:= value) -> value
unField _ (_ R.:= value) = value

----------------------------------------------------------------------------
-- Aliases
----------------------------------------------------------------------------

type a :=: b = a := b

pattern a :=: b = a R.:= b

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
  Record lhs ->
  Record rhs ->
  Record res
union (MkRecord a) (MkRecord b) = MkRecord (R.combine a b)

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
  Generic (Record fields)
  where
  type
    Rep (Record fields) =
      RecordRep fields
  from (MkRecord r) = R.toNative' r
  to rep = MkRecord (R.fromNative' rep)

----------------------------------------------------------------------------
-- generic-lens
----------------------------------------------------------------------------

instance {-# OVERLAPPING #-} (R.Set field fields a' ~ fields', R.Set field fields' a ~ fields, R.Has field fields a, R.Has field fields' a') => GL.HasField field (Record fields) (Record fields') a a' where
  field = coerced @(Record fields) @_ @(R.Rec fields) @_ . R.lens (R.FldProxy @field)

instance {-# OVERLAPPING #-} (R.Set field fields a ~ fields, R.Has field fields a) => GL.HasField' field (Record fields) a where
  field' = coerced @(Record fields) @_ @(R.Rec fields) @_ . R.lens (R.FldProxy @field)

pattern ExactRecord :: RecTuple tuple fields => tuple -> Record fields
pattern ExactRecord a <-
  MkRecord (toTuple -> a)
  where
    ExactRecord = MkRecord . fromTuple

pattern Record ::
  ( RecTuple tuple fields,
    R.RecCopy fields fields fields',
    R.RecCopy fields' fields' fields
  ) =>
  tuple ->
  Record fields'
pattern Record a <-
  MkRecord (toTuple . R.recCopy -> a)
  where
    Record = MkRecord . R.recCopy . fromTuple
