{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances  #-}

module App where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except
import           Data.ByteString (ByteString)
import           Data.List
import           Data.Map
import           Data.Monoid
import           Data.Time.Calendar (fromGregorian)
import qualified Data.Text as T

import           Network.Wai
import           Database.Persist
import           Servant
import           Servant.Server.Experimental.Auth
import           Servant.API.Experimental.Auth
import           Web.Cookie

import           Api
import           Utils


newtype Account = Account { unAccount :: T.Text }

database :: Map ByteString Account
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghdalia Tazarts")
                    ]

lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Data.Map.lookup key database of
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
  Just usr -> return usr

authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ do
    cookie <- maybeToEither "Missing cookie header" $ Data.List.lookup "cookie" $ requestHeaders req
    maybeToEither "Missing token in cookie" $ Data.List.lookup "klaraworks-admin" $ parseCookies cookie
    
type instance AuthServerData (AuthProtect "cookie-auth") = Account

genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext


klaraWorksApp :: IO Application
klaraWorksApp = return $ serveWithContext klaraWorksApi genAuthServerContext server

server :: Server KlaraWorksApi
server = getWorksList :<|>
         getWorks :<|>
         postWorks :<|>
         putWorks :<|>
         deleteWorks 

getWorksList ::  Handler [ApiWorks]
getWorksList = do
  liftIO $ runSql $ do
    worksList <- selectList [] []
    return $ Data.List.map entityToApiWorks worksList

getWorks :: T.Text -> Handler ApiWorks
getWorks inDir = do
  works <- liftIO $ runSql $ do
    works <- selectFirst [WorksDir ==. inDir] []
    return works
  case works of
    Just w -> return $ entityToApiWorks w
    Nothing -> throwError err404 { errBody = "Entry not found" }

postWorks :: Account -> ApiWorks -> Handler ()
postWorks acc w = do
  let record = apiWorksToModel w
  let inDir = dir w 
  exists <- liftIO $ runSql $ selectFirst [WorksDir ==. inDir] []
  if exists == Nothing then do
    liftIO $ runSql $ Database.Persist.insert record
    return ()
    else
    throwError err409 { errBody = "Entry already exists." }

putWorks :: Account -> T.Text -> ApiWorks -> Handler ()
putWorks acc inDir w = do
  let record = apiWorksToModel w
  exists <- liftIO $ runSql $ selectFirst [WorksDir ==. inDir] []
  case exists of
    Just (Entity wid _) -> do
      liftIO $ runSql $ replace wid record
      return ()
    Nothing -> throwError err404 { errBody = "Entry not found." }

deleteWorks :: Account -> T.Text -> Handler ()
deleteWorks acc inDir = liftIO $ runSql $ deleteWhere [WorksDir ==. inDir]
