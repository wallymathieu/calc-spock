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



type State = STM (TVar [Calculation])
type Api = SpockM () () State ()

type ApiAction a = SpockAction () () State a

getAll :: ApiAction [Calculation]
getAll = do
  data' <- getState
  liftIO (atomically data' >>= readTVarIO)

addCalc :: Calculation -> ApiAction ()
addCalc calc = do
  data' <- getState
  liftIO (atomically data' >>= doAddCalc calc)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x

doAddCalc :: Calculation -> TVar [Calculation] -> IO ()
doAddCalc calc state = atomically (modifyTVar' state addToList)
  where
    addToList s = calc : s

addCalcAction :: ApiAction a
addCalcAction = do
    calc <- jsonBody' :: ApiAction Calculation
    res <- addCalc calc
    json res

getAllAction :: ApiAction a
getAllAction = do
    calcs <- getAll
    json calcs

sampleAction :: ApiAction a
sampleAction =
  let one = Const 1
      firstCalc = BinaryExpression Plus one one in
  json firstCalc

app :: Api
app = do
  get "" sampleAction
  get "calculations" getAllAction
  --get ("auction" <//> var) getAuctionAction
  post "add" addCalcAction
 