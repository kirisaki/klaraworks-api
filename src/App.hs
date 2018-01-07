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
  getWorks

getWorksList :: Handler [ApiWorks]
getWorksList = do
  liftIO $ runSql $ do
    worksList <- selectList [] []
    return $ map toApiWorksFE worksList

getWorks :: T.Text -> Handler ApiWorks
getWorks str = do
  works <- liftIO $ runSql $ do
    works <- selectFirst [WorksDir ==. str] []
    return works
  case works of
    Just w -> return $ toApiWorksFE w
    Nothing -> throwError err404

                  ]
