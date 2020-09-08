module JRec.Generic where

import GHC.Generics
import JRec.Internal (Rec, (:=) (..))
import qualified JRec.Internal as R

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
