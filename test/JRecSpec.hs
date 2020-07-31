module JRecSpec (spec) where

import Test.Hspec
import Control.Lens ((&), (^.), (.~))
import JRec

spec :: Spec
spec = do 
  it "polymorphic" $ do
    (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6) & #u .~ 5)
      `shouldBe`  ExactRecord (#u :=: 5, #a :=: 5, #b :=: 6)
  it "show" $ do
    show (ExactRecord ()) `shouldBe` "{}"
    show (ExactRecord (#foo :=: True)) `shouldBe` "{foo = True}"
    show (ExactRecord (#foo :=: True, #bar :=: 0)) `shouldBe` "{foo = True, bar = 0}"
  it "get" $ do 
    let getA1 :: Record ("a" :=: Int ': rest) -> Int
        getA1 = (^. #a)
    let getA2 :: Record ("u" :=: Bool ': "a" :=: Int ': rest) -> Int
        getA2 = (^. #a)
    getA1 (ExactRecord (#a :=: 5)) `shouldBe` 5
    getA1 (ExactRecord (#a :=: 5, #b :=: 6)) `shouldBe` 5
    getA2 (ExactRecord (#u :=: True, #a :=: 5)) `shouldBe` 5
    getA2 (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6)) `shouldBe` 5
  it "set" $ do
    let setA1 ::
          Record ("a" :=: Int ': rest) ->
          Record ("a" :=: Int ': rest)
        setA1 = (#a .~ 8)
    let setA2 ::
          Record ("u" :=: Bool ': "a" :=: Int ': rest) ->
          Record ("u" :=: Bool ': "a" :=: Int ': rest)
        setA2 = (#a .~ 8)
    setA1 (ExactRecord (#a :=: 5))
      `shouldBe` (ExactRecord (#a :=: 8))
    setA1 (ExactRecord (#a :=: 5, #b :=: 6))
      `shouldBe` (ExactRecord (#a :=: 8, #b :=: 6))
    setA2 (ExactRecord (#u :=: True, #a :=: 5))
      `shouldBe` (ExactRecord (#u :=: True, #a :=: 8))
    setA2 (ExactRecord (#u :=: True, #a :=: 5, #b :=: 6))
      `shouldBe` (ExactRecord (#u :=: True, #a :=: 8, #b :=: 6))


