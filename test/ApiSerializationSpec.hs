{-# LANGUAGE OverloadedStrings #-}
module ApiSerializationSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import Data.Time
import Domain

spec:: ()->SpecWith ()
spec ()=do

  describe "calculation serialization" $ do
    let one = Const 1
    let firstCalc = BinaryExpression Plus one one
    let firstCalcJson = encode firstCalc

    it "can understand first auction" $
      let actual = decode firstCalcJson in
        actual `shouldBe` Just firstCalc

