{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except
import           Data.List
import           Data.Time.Calendar (fromGregorian)
import qualified Data.Text as T

import           Network.Wai
import           Database.Persist
import           Servant

import           Api
import           Utils

klaraWorksApp :: IO Application
klaraWorksApp = return $ serve klaraWorksApi server

server :: Server KlaraWorksApi
server =
  getWorksList :<|>
  getWorks :<|>
  postWorks

getWorksList :: Handler [ApiWorks]
getWorksList = do
  liftIO $ runSql $ do
    worksList <- selectList [] []
    return $ map entityToApiWorks worksList

getWorks :: T.Text -> Handler ApiWorks
getWorks str = do
  works <- liftIO $ runSql $ do
    works <- selectFirst [WorksDir ==. str] []
    return works
  case works of
    Just w -> return $ entityToApiWorks w
    Nothing -> throwError err404

postWorks :: ApiWorksReq -> Handler ()
postWorks w = do
  let record = reqToModel w
  let dir = reqDir w 
  exists <- liftIO $ runSql $ selectFirst [WorksDir ==. dir] []
  if exists == Nothing then do
    liftIO $ runSql $ Database.Persist.insert record
    return ()
    else
    throwError err409 { errBody = "Entry already exists." }
