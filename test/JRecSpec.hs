module JRecSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import JRec
import Test.Hspec
import qualified JRec.Internal as R

type Foo  = Rec '["a" := Integer, "b" := Integer, "u" := Integer]

foo :: Foo 
foo = Rec (#u := 1, #a := 2, #b := 3)

spec :: Spec
spec = do
  it "polymorphic" $ do
    let x :: Foo
          = (Rec (#a := 5, #b := 6, #u := 2)) -- & #u .~ 5)
        y :: Foo
          = Rec (#a := 5, #b := 6, #u := 5)
    x `shouldBe` y
  it "show" $ do
    show (Rec ()) `shouldBe` "{}"
    show (Rec (#foo := True)) `shouldBe` "{foo = True}"
    show (Rec (#foo := True, #bar := 0, #tas := True)) `shouldBe` "{foo = True, bar = 0}"
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
        setA1 = R.set #a 8
    let setA2 ::
          Rec ("u" := Bool ': "a" := Int ': rest) ->
          Rec ("u" := Bool ': "a" := Int ': rest)
        setA2 = (#a .~ 8)
    setA1 (Rec (#a := 5))
      `shouldBe` (Rec (#a := 8))
    setA1 (Rec (#a := 5, #b := 6))
      `shouldBe` (Rec (#a := 8, #b := 6))
    --setA2 (Rec (#u := True, #a := 5))
    --  `shouldBe` (Rec (#u := True, #a := 8))
    setA2 (Rec (#u := True, #a := 5, #b := 6))
      `shouldBe` (Rec (#u := True, #a := 8, #b := 6))
  describe "append" $ do
    it "simple append" $ do
      Rec (#a := 1) `append` Rec (#b := 2)
        `shouldBe` Rec (#a := 1, #b := 2)
    it "append with duplicates" $ do
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `union` r2
        `shouldBe` Rec (#c := 7, #b := 5, #a := 8)
        -- `shouldBe` Rec (#b := 5, #a := 8, #c := 7, #a := 8)
  describe "union" $ do
    it "simple union" $ do
      Rec (#a := 1) `union` Rec (#b := 2)
        `shouldBe` Rec (#a := 1, #b := 2)
    it "union with duplicates (not implemented)" $ do
      -- pendingWith "TODO: Remove duplicates"
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `union` r2
        `shouldBe` Rec (#c := 7, #b := 5, #a := 8)
  describe "insertOrSet" $ do
    it "simple insertOrSet" $ do
      insertOrSet (#a := 1) (Rec (#b := 2))
        `shouldBe` Rec (#a := 1, #b := 2)
    it "insertOrSet that replaces" $ do
      pendingWith "TODO: Replace should replace"
      insertOrSet (#a := 1) (Rec (#b := 2, #a := 0))
        `shouldBe` Rec (#b := 2, #a := 1)
