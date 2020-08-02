module JRecSpec (spec) where

import Control.Lens ((&), (.~), (^.))
import JRec
import Test.Hspec

spec :: Spec
spec = do
  it "polymorphic" $ do
    (Rec (#u := True, #a := 5, #b := 6) & #u .~ 5)
      `shouldBe` Rec (#u := 5, #a := 5, #b := 6)
  it "show" $ do
    show (Rec (#foo := True)) `shouldBe` "{foo = True}"
    show (Rec (#foo := True, #bar := 0)) `shouldBe` "{foo = True, bar = 0}"
    show (Rec ()) `shouldBe` "{}"
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
