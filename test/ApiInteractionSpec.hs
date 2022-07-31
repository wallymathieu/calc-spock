{-# LANGUAGE OverloadedStrings #-}
module ApiInteractionSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import Data.Time
import Domain
import Web (app, AppState (AppState, added), initAppState)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp, spock)
import Web.Spock.Config
import GHC.Conc

one = Const 1
firstCalc = BinaryExpression Plus one one
firstCalcJson = encode firstCalc

configuredApp = do
  state <- initAppState
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  spock spockCfg app
spec :: Spec
spec =
    with (spockAsApp configuredApp) $
        do describe "GET /" $
            
            do it "serves the home page" $ get "/" `shouldRespondWith` "\"Hello world\"" {matchStatus = 200}
           describe "POST data" $
               do it "returns hello to spock" $
                      post "/" "\"spock\"" `shouldRespondWith` "\"spock\""
                  it "returns hello to uhura" $
                      post "/" "\"uhura\"" `shouldRespondWith` "\"uhura\""
                  it "returns previous" $ do
                      post "/" "\"uhura\""
                      get "/all" `shouldRespondWith` "[\"uhura\"]"
