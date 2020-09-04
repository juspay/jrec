{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module JRec
  ( unField,
    (:=) (..),
    Rec,
    pattern Rec,
    append,
    union,
    insert,
    insertOrSet,
  )
where

import Control.Lens (coerced, (&), (.~), (^.))
import qualified "generic-lens" Data.Generics.Product.Fields as GL
import qualified "generic-optics" Data.Generics.Product.Fields as GO
import qualified "generic-lens" Data.Generics.Wrapped as GL
import qualified "generic-optics" Data.Generics.Wrapped as GO
import Data.Proxy
import GHC.Exts (Any)
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits
import Generic.Data
import JRec.Internal (Rec, append, (:=) (..))
import qualified JRec.Internal as R
import JRec.Tuple
import Unsafe.Coerce

-- $setup
--
-- >>> :set -XOverloadedLabels

----------------------------------------------------------------------------
-- unField
----------------------------------------------------------------------------

unField :: field ~ field' => R.FldProxy field -> (field' R.:= value) -> value
unField _ (_ R.:= value) = value

----------------------------------------------------------------------------
-- Other operations
----------------------------------------------------------------------------

-- | Merges records, removing duplicates
--
-- Left-biased. Does not sort.
--
-- O(n * m) type check complexity.
union ::
  forall lhs rhs res.
  ( KnownNat (R.RecSize lhs),
    KnownNat (R.RecSize rhs),
    KnownNat (R.RecSize res),
    res ~ R.Union lhs rhs,
    R.RecCopy lhs lhs res,
    R.RecCopy rhs rhs res
  ) =>
  Rec lhs ->
  Rec rhs ->
  Rec res
union = R.union

-- | Insert a field into a record.
--
-- O(n) type check complexity.
--
-- Will fail at compile time if the record already contains the field:
--
-- >>> (#a := '1') `insert` Rec (#b := '2', #a := '0')
-- ...
-- ... Duplicate key "a"
-- ...
insert ::
  forall label value lts res.
  ( KnownNat (1 + R.RecSize lts),
    KnownNat (R.RecSize lts),
    KnownSymbol label,
    R.RecCopy lts lts res,
    res ~ ((label := value) : lts),
    R.KeyDoesNotExist label lts
  ) =>
  label := value ->
  Rec lts ->
  Rec res
insert = R.append . Rec

-- | Insert a field into a record. Set it if it already exists
--
-- O(n) type check complexity.
insertOrSet ::
  forall label value rhs res.
  ( KnownNat (R.RecSize rhs),
    KnownNat (R.RecSize res),
    KnownNat (R.RecTyIdxH 0 label res),
    KnownSymbol label,
    value ~ R.RecTy label res,
    R.Reverse (R.Insert (label := value) (R.Reverse rhs)) ~ res,
    R.RecCopy rhs rhs res
  ) =>
  label := value ->
  Rec rhs ->
  Rec res
insertOrSet = R.insert

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

----------------------------------------------------------------------------
-- generic-optics
----------------------------------------------------------------------------

instance {-# OVERLAPPING #-} (R.Set field fields a' ~ fields', R.Set field fields' a ~ fields, R.Has field fields a, R.Has field fields' a') => GO.HasField field (Rec fields) (Rec fields') a a' where
  field = R.opticLens (R.FldProxy @field)

instance {-# OVERLAPPING #-} (R.Set field fields a ~ fields, R.Has field fields a) => GO.HasField' field (Rec fields) a where
  field' = R.opticLens (R.FldProxy @field)

pattern Rec ::
  ( RecTuple tuple fields
  ) =>
  tuple ->
  Rec fields
pattern Rec a <-
  (toTuple -> a)
  where
    Rec = fromTuple
