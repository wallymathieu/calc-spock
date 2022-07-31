{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           GHC.Generics
import           GHC.Conc        (TVar,newTVar,readTVar,readTVarIO,writeTVar,atomically)
import           GHC.Conc.Sync   (STM)
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text       as T
import qualified Data.List       as L
import           Web             (app)



main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase (newTVar [])
  runSpock 8080 (spock spockCfg app)
