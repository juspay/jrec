module JRec.Field where

import qualified Data.Generics.Labels as GL
import qualified Data.Generics.Product.Fields as GL
import GHC.TypeLits

type Field' (s :: Symbol) a b = (GL.HasField' s a b, GL.Field' s a b)
