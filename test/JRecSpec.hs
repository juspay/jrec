module JRecSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import GHC.Stack
import JRec
import Test.Hspec

data Pair a = Pair !a !a
  deriving (Eq, Show)

spec :: HasCallStack => Spec
spec = do
  it "is not crazy" $ do
    Pair (Rec (#a := 1)) (Rec (#a := 2))
      `shouldNotBe` Pair (Rec (#a := 1)) (Rec (#a := 1))
    Pair (Rec (#a := 1)) (Rec (#a := 2))
      `shouldNotBe` Pair (Rec (#a := 2)) (Rec (#a := 2))
  it "polymorphic" $ do
    (Rec (#u := True, #a := 5, #b := 6, #a := 2) & #u .~ 5)
      `shouldBe` Rec (#u := 5, #a := 5, #b := 6, #a := 2)
  describe "eq" $ do
    -- eq can compare only the first matching field, discarding the rest.
    it "fails if first matching field doesn't compare" $ do
      Rec (#a := 1, #a := 2) `shouldNotBe` Rec (#a := 0, #a := 2)
    it "succeeds if first matching field compares" $ do
      Rec (#a := 1, #a := 2) `shouldBe` Rec (#a := 1, #a := 0)
  it "ord" $ do
    -- Same as eq when there is duplicated keys.
    -- Only the first occurence of that key is considered.
    compare (Rec (#a := 1, #a := 2)) (Rec (#a := 1, #a := 2)) `shouldBe` EQ
    compare (Rec (#a := 1, #a := 2)) (Rec (#a := 1, #a := 3)) `shouldBe` EQ
    compare (Rec (#a := 1, #a := 2)) (Rec (#a := 2, #a := 1)) `shouldBe` LT
    compare (Rec (#a := 1, #a := 2)) (Rec (#a := 0, #a := 3)) `shouldBe` GT
    compare (Rec (#a := 1, #b := 2)) (Rec (#a := 1, #b := 2)) `shouldBe` EQ
    compare (Rec (#a := 1, #b := 2)) (Rec (#a := 1, #b := 3)) `shouldBe` LT
    compare (Rec (#a := 1, #b := 2)) (Rec (#a := 2, #b := 1)) `shouldBe` LT
    compare (Rec (#a := 1, #b := 2)) (Rec (#a := 0, #b := 3)) `shouldBe` GT
    compare (Rec (#a := 1, #a := 2, #b := 3)) (Rec (#a := 1, #a := 3, #b := 2)) `shouldBe` GT
  it "show" $ do
    show (Rec ()) `shouldBe` "{}"
    show (Rec (#foo := True)) `shouldBe` "{foo = True}"
    show (Rec (#foo := True, #bar := 0)) `shouldBe` "{foo = True, bar = 0}"
  it "get" $ do
    let getA1 :: Rec ("a" := Int ': rest) -> Int
        getA1 = (^. #a)
    let getA2 :: Rec ("u" := Bool ': "a" := Int ': rest) -> Int
        getA2 = (^. #a)
    getA1 (Rec (#a := 5)) `shouldBe` 5
    getA1 (Rec (#a := 5, #b := 6)) `shouldBe` 5
    getA2 (Rec (#u := True, #a := 5)) `shouldBe` 5
    getA2 (Rec (#u := True, #a := 5, #b := 6)) `shouldBe` 5
  it "set" $ do
    let setA1 ::
          Rec ("a" := Int ': rest) ->
          Rec ("a" := Int ': rest)
        setA1 = (#a .~ 8)
    let setA2 ::
          Rec ("u" := Bool ': "a" := Int ': rest) ->
          Rec ("u" := Bool ': "a" := Int ': rest)
        setA2 = (#a .~ 8)
    setA1 (Rec (#a := 5))
      `shouldBe` (Rec (#a := 8))
    setA1 (Rec (#a := 5, #b := 6))
      `shouldBe` (Rec (#a := 8, #b := 6))
    setA2 (Rec (#u := True, #a := 5))
      `shouldBe` (Rec (#u := True, #a := 8))
    setA2 (Rec (#u := True, #a := 5, #b := 6))
      `shouldBe` (Rec (#u := True, #a := 8, #b := 6))
  describe "append" $ do
    it "simple append" $ do
      Rec (#a := 1) `append` Rec (#b := 2)
        `shouldBe` Rec (#a := 1, #b := 2)
      Rec () `append` Rec (#b := 2)
        `shouldBe` Rec (#b := 2)
      Rec (#a := 1) `append` Rec ()
        `shouldBe` Rec (#a := 1)
      Rec (#a := 1, #b := 2) `append` Rec (#c := 3)
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
      Rec (#a := 1) `append` Rec (#b := 2, #c := 3)
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
      Rec (#a := 1, #b := 2) `append` Rec (#c := 3, #d := 4)
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3, #d := 4)
    it "append with duplicates" $ do
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `append` r2
        `shouldBe` Rec (#b := 5, #a := 6, #c := 7, #a := 8)
  describe "union" $ do
    it "simple union" $ do
      Rec (#a := 1) `union` Rec (#b := 2)
        `shouldBe` Rec (#a := 1, #b := 2)
    it "union with duplicates (left-biased)" $ do
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `union` r2
        `shouldBe` Rec (#b := 5, #a := 6, #c := 7)
  describe "insert" $ do
    it "simple insert" $ do
      (#a := 1) `insert` Rec (#b := 2, #c := 3)
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
  describe "insertOrSet" $ do
    it "distinct" $ do
      insertOrSet (#a := 1) (Rec (#b := 2, #c := 3))
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
    it "overwrite" $ do
      insertOrSet (#c := 1) (Rec (#b := 2, #c := 3))
        `shouldBe` Rec (#b := 2, #c := 1)
  describe "optional fields" $ do
    it "encode" $ do
      encode (Rec (#a := (1 :: Int), #b := (Nothing :: Maybe Int)))
        `shouldBe` "{\"a\":1}"
    it "encode" $ do
      encode (Rec (#a := (1 :: Int), #b := (Just 2 :: Maybe Int)))
        `shouldBe` "{\"a\":1,\"b\":2}"
    it "decode" $ do
      decode "{\"a\": 1}"
        `shouldBe` Just (Rec (#a := (1 :: Int), #b := (Nothing :: Maybe Int)))
      decode "{\"a\": 1, \"b\": null}"
        `shouldBe` Just (Rec (#a := (1 :: Int), #b := (Nothing :: Maybe Int)))
      decode "{\"a\": 1, \"b\": 2}"
        `shouldBe` Just (Rec (#a := (1 :: Int), #b := (Just 2 :: Maybe Int)))
