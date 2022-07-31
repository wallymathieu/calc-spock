{-# LANGUAGE OverloadedStrings #-}
module ApiInteractionSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import Data.Time
import Domain
import Web (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp, spock)
import Web.Spock.Config
import GHC.Conc

one = Const 1
firstCalc = BinaryExpression Plus one one
firstCalcJson = encode firstCalc

configuredApp = do
  spockCfg <- defaultSpockCfg () PCNoDatabase (newTVar [])
  spock spockCfg app
spec :: Spec
spec =
    with (spockAsApp configuredApp) $
        do describe "GET /" $
            
            do it "serves the home page" $ get "/" `shouldRespondWith` "firstCalcJson" {matchStatus = 200}
           {-describe "GET /hello/:name" $
               do it "returns hello to spock" $
                      get "/hello/spock" `shouldRespondWith` "Hello spock"
                  it "returns hello to uhura" $
                      get "/hello/uhura" `shouldRespondWith` "Hello uhura"
          -}
