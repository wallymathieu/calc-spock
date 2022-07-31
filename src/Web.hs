{-# LANGUAGE OverloadedStrings #-}
module Web where

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
import           Domain



data AppState = AppState { added :: TVar [String] }

initAppState :: IO AppState
initAppState = atomically $ do
    added <- newTVar []
    return (AppState {added=added})

type Api = SpockM () () AppState ()

type ApiAction a = SpockAction () () AppState a

getAll :: ApiAction [String]
getAll = do
  data' <- getState 
  liftIO (readTVarIO $ added data')

add :: String -> ApiAction String
add value = do
  data' <- getState
  liftIO (doAdd value data')
  return value
  where
    doAdd :: String -> AppState -> IO ()
    doAdd value AppState { added = state } = atomically $ do 
        current <- readTVar state
        writeTVar state (value : current)

addAction :: ApiAction a
addAction = (jsonBody' :: ApiAction String) >>= add >>= json

getAllAction :: ApiAction a
getAllAction = getAll >>= json
helloWorldText :: String
helloWorldText = "Hello world"
sampleAction :: ApiAction a
sampleAction = json helloWorldText

app :: Api
app = do
  get "" sampleAction
  get "all" getAllAction
  post "" addAction
 