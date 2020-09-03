{-# OPTIONS_GHC -fdefer-type-errors #-}

module JRecShouldNotTypecheckSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import JRec
import Test.Hspec
import Test.ShouldNotTypecheck
import GHC.Stack

spec :: Spec
spec = do
  describe "insert" $ do
    it "type-check fails if field already exists" $ do
      shouldNotTypecheck ((#a := '1') `insert` Rec (#b := '2', #a := '0'))
