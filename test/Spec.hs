{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Spec where

import Test.Hspec
import Lib

one = "   \
      \  |\
      \  |"

testCreateDigit::String -> Int -> Spec
testCreateDigit inputstring value = createDigit inputstring `shouldBe` value

main :: IO ()
main = hspec $ do
  describe "scoreGame" $ do
    testCreateDigit one 1