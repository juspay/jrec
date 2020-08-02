module JRec.Field where

import qualified "generic-lens" Data.Generics.Labels as GL
import qualified "generic-lens" Data.Generics.Product.Fields as GL
import GHC.TypeLits

type Field' (s :: Symbol) a b = (GL.HasField' s a b, GL.Field' s a b)
