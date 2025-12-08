module Day1Spec
  ( spec,
  )
where

import Day1 (numPassByZero)
import Test.Hspec

spec :: Spec
spec = do
  it "should have no zero between 20 and 50" $ do
    (numPassByZero 20 50) `shouldBe` 0
  it "should have one zero between 20 and 140" $ do
    (numPassByZero 20 140) `shouldBe` 1
  it "should have three zeros between 20 and 340" $ do
    (numPassByZero 20 340) `shouldBe` 3
  it "should have one zero between 20 and -20" $ do
    (numPassByZero 20 (-20)) `shouldBe` 1
  it "should have one zero between -20 and 20" $ do
    (numPassByZero (-20) 20) `shouldBe` 1
  it "should have one zero between -20 and -100" $ do
    (numPassByZero (-20) (-100)) `shouldBe` 1
  it "should have no zero between -100 and -20" $ do
    (numPassByZero (-100) (-20)) `shouldBe` 0
  it "should have three zeros between 20 and -201" $ do
    (numPassByZero 20 (-201)) `shouldBe` 3
  it "should have one zero between 20 and 0" $ do
    (numPassByZero 20 0) `shouldBe` 1
  it "should have no zero between 0 and 20" $ do
    (numPassByZero 0 20) `shouldBe` 0
  it "should have one zero between 20 and 100" $ do
    (numPassByZero 20 100) `shouldBe` 1
  it "should have no zero between 100 and 20" $ do
    (numPassByZero 100 20) `shouldBe` 0
  it "should have one zero between 0 and 100" $ do
    (numPassByZero 0 100) `shouldBe` 1
  it "should have one zero between -20 and 0" $ do
    (numPassByZero (-20) 0) `shouldBe` 1
  it "should have no zero between 0 and -20" $ do
    (numPassByZero 0 (-20)) `shouldBe` 0
  it "should have one zero between 0 and 100" $ do
    (numPassByZero 0 100) `shouldBe` 1
  it "should have one zero between 100 and 0" $ do
    (numPassByZero 100 0) `shouldBe` 1
  it "should have one zero between -100 and 0" $ do
    (numPassByZero (-100) 0) `shouldBe` 1
  it "should have one zero between 0 and -100" $ do
    (numPassByZero (-100) 0) `shouldBe` 1
  it "should have two zeros between 100 and -100" $ do
    (numPassByZero 100 (-100)) `shouldBe` 2
  it "should have two zeros between -100 and 100" $ do
    (numPassByZero (-100) 100) `shouldBe` 2
  it "should have two zeros between 100 and 300" $ do
    (numPassByZero 100 300) `shouldBe` 2
  it "should have two zeros between 300 and 100" $ do
    (numPassByZero 300 100) `shouldBe` 2
  it "should have two zeros between -100 and -300" $ do
    (numPassByZero (-100) (-300)) `shouldBe` 2
  it "should have two zeros between -300 and -100" $ do
    (numPassByZero (-300) (-100)) `shouldBe` 2
  it "should have three zeros between -301 and -20" $ do
    (numPassByZero (-301) (-20)) `shouldBe` 3
  it "should have two zeros between -350 and -101" $ do
    (numPassByZero (-350) (-101)) `shouldBe` 2
  it "should have three zeros between 20 and 301" $ do
    (numPassByZero 20 301) `shouldBe` 3
  it "should have two zeros between 101 and 350" $ do
    (numPassByZero 99 250) `shouldBe` 2
