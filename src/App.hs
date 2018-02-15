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

import           Control.Lens hiding ((:>))
import           Network.Wai
import           Data.Extensible
import           Database.Persist
import           Servant
import           Servant.Server.Experimental.Auth
import           Servant.API.Experimental.Auth
import           Web.Cookie

import           Api
import           Utils
import           Types


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

genAuthServerContext :: Servant.Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext


klaraWorksApp :: IO Application
klaraWorksApp = return $ serveWithContext klaraWorksApi genAuthServerContext server

server :: Server KlaraWorksApi
server = getWorksList :<|>
         getWorks :<|>
         getInfoList :<|>
         getInfo :<|>
         postInfo :<|>
         putInfo :<|>
         deleteInfo :<|> 
         getDetailList :<|>
         getDetail :<|>
         postDetail :<|>
         putDetail :<|>
         deleteDetail 

getWorksList :: T.Text -> Handler [ApiWorksHeader]
getWorksList language = return [test]
  where
    test :: ApiWorksHeader
    test = dir @= "aaaaaa"
      <: title @= "aaa"
      <: date @= (fromGregorian 2018 02 04)
      <: nil
      
getWorks :: T.Text -> Dir -> Handler ApiWorks
getWorks language inDir = undefined

getInfoList :: Account -> Handler [ApiInfo]
getInfoList acc = undefined

getInfo :: Account -> Dir -> Handler ApiInfo
getInfo acc infoDir = undefined

postInfo :: Account -> ApiInfo -> Handler()
postInfo acc info = undefined

putInfo :: Account -> Dir -> ApiInfo -> Handler ()
putInfo acc infoDir info = undefined

deleteInfo :: Account -> Dir -> Handler ()
deleteInfo acc infoDir  = undefined

getDetailList :: Account -> Handler [ApiDetail]
getDetailList acc = undefined

getDetail :: Account -> Dir -> Handler ApiDetail
getDetail acc detailDir = undefined

postDetail :: Account -> ApiDetail -> Handler()
postDetail acc detail = undefined

putDetail :: Account -> Dir -> ApiDetail -> Handler ()
putDetail acc detailDir detail = undefined

deleteDetail :: Account -> Dir -> Handler ()
deleteDetail acc detailDir  = undefined

