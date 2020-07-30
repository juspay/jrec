{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Record
  ( Record,
    pattern Record,
    (:=:),
    pattern (:=:),
    pattern ExactRecord,
    unField,
    union,

    -- * Tests
    unit_polymorphic,
    unit_show,
    unit_get,
    unit_set,
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
import qualified Record.Super as R
import Control.Lens ((&), (^.))
import Record.Super ((:=))
import Record.Tuple
import Test.Tasty.HUnit
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
    ('MetaData "Record" "Record" "" 'False)
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

unit_polymorphic :: IO ()
unit_polymorphic = do
  (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6) & #u .~ 5)
    @?= ExactRecord (#u :=: 5, #a :=: 5, #b :=: 6)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

unit_show :: IO ()
unit_show = do
  show (ExactRecord ()) @?= "{}"
  show (ExactRecord (#foo :=: True)) @?= "{foo = True}"
  show (ExactRecord (#foo :=: True, #bar :=: 0)) @?= "{foo = True, bar = 0}"

unit_get :: IO ()
unit_get = do
  let getA1 :: Record ("a" :=: Int ': rest) -> Int
      getA1 = (^. #a)
  let getA2 :: Record ("u" :=: Bool ': "a" :=: Int ': rest) -> Int
      getA2 = (^. #a)
  getA1 (ExactRecord (#a :=: 5)) @?= 5
  getA1 (ExactRecord (#a :=: 5, #b :=: 6)) @?= 5
  getA2 (ExactRecord (#u :=: True, #a :=: 5)) @?= 5
  getA2 (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6)) @?= 5

unit_set :: IO ()
unit_set = do
  let setA1 ::
        Record ("a" :=: Int ': rest) ->
        Record ("a" :=: Int ': rest)
      setA1 = (#a .~ 8)
  let setA2 ::
        Record ("u" :=: Bool ': "a" :=: Int ': rest) ->
        Record ("u" :=: Bool ': "a" :=: Int ': rest)
      setA2 = (#a .~ 8)
  setA1 (ExactRecord (#a :=: 5))
    @?= (ExactRecord (#a :=: 8))
  setA1 (ExactRecord (#a :=: 5, #b :=: 6))
    @?= (ExactRecord (#a :=: 8, #b :=: 6))
  setA2 (ExactRecord (#u :=: True, #a :=: 5))
    @?= (ExactRecord (#u :=: True, #a :=: 8))
  setA2 (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6))
    @?= (ExactRecord (#u :=: True, #a :=: 8, #b :=: 6))

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
