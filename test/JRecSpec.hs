module JRecSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import JRec
import Test.Hspec

spec :: Spec
spec = do
  it "polymorphic" $ do
    (Rec (#u := True, #a := 5, #b := 6, #a := 2 ) & #u .~ 5)
      `shouldBe` Rec (#u := 5, #a := 5, #b := 6, #a := 2)
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
    it "append with duplicates" $ do
      pendingWith "append is overwriting all duplicate fields with same value"
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `append` r2
        `shouldBe` Rec (#b := 5, #a := 6, #c := 7, #a := 8)
  describe "union" $ do
    it "simple union" $ do
      Rec (#a := 1) `union` Rec (#b := 2)
        `shouldBe` Rec (#a := 1, #b := 2)
    it "union with duplicates" $ do
      let r1 = Rec (#b := 5, #a := 6)
          r2 = Rec (#c := 7, #a := 8)
      r1 `union` r2
        `shouldBe` Rec (#b := 5, #a := 6, #c := 7)
  describe "insert" $ do 
    it "simple insert" $ do 
      (#a := 1) `insert` Rec (#b := 2, #c := 3)
        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
    it "duplicate insert should be disallowed" $ do 
      pendingWith "TODO"
--  describe "insertOrSet" $ do
--    it "distinct" $ do
--      insertOrSet (#a := 1) (Rec (#b := 2, #c := 3))
--        `shouldBe` Rec (#a := 1, #b := 2, #c := 3)
--    it "overwrite" $ do
--      insertOrSet (#c := 1) (Rec (#b := 2, #c := 3))
--        `shouldBe` Rec (#b := 2, #c := 1)
