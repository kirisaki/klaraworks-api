{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.TH
import           Database.Persist
import           Database.Persist.TH
import           Servant.API
import           Servant.API.Experimental.Auth
import           Servant.Elm(ElmType)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Works
        dir T.Text
        title T.Text
        date Day
        event T.Text
        worksType T.Text
        origin T.Text
        fanart Bool
        contents [T.Text]
        status T.Text
        UniqueHandler dir
        deriving Show Eq
|]

data ApiWorks = ApiWorks 
  { dir :: T.Text
  , title :: T.Text
  , date :: Day
  , event :: Maybe T.Text
  , worksType :: T.Text
  , origin :: Maybe T.Text
  , fanart :: Bool
  , contents :: [T.Text]
  , status :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)
    
modelToApiWorks :: Works -> ApiWorks
modelToApiWorks w = ApiWorks
  { dir = worksDir w
  , title = worksTitle w
  , date = worksDate w
  , event = Just $ worksEvent w
  , worksType = worksWorksType w
  , origin = Just $ worksOrigin w
  , fanart = worksFanart w
  , contents = worksContents w
  , status = Just $ worksStatus w
  }

apiWorksToModel :: ApiWorks -> Works
apiWorksToModel w = let
  emptyString s =
    case s of
      Just str -> str
      Nothing -> ""
  in
    Works
    { worksDir = dir w
    , worksTitle = title w
    , worksDate = date w
    , worksEvent = emptyString $ event w
    , worksWorksType = worksType w
    , worksOrigin = emptyString $ origin w
    , worksFanart =  fanart w
    , worksContents = contents w
    , worksStatus = emptyString $ status w
    }
    
entityToApiWorks :: Entity Works -> ApiWorks
entityToApiWorks (Entity wid w) = modelToApiWorks w  

instance ToJSON ApiWorks
instance FromJSON ApiWorks
instance ElmType ApiWorks

type KlaraWorksApi =
  "_api" :>
  ( "works" :>
    ( Get '[JSON] [ApiWorks] :<|>
      Capture "worksDir" T.Text :> Get '[JSON] ApiWorks :<|>
      AuthProtect "cookie-auth" :> ReqBody '[JSON] ApiWorks :> Post '[JSON] () :<|>
      AuthProtect "cookie-auth" :> Capture "worksDir" T.Text :> ReqBody '[JSON] ApiWorks :> Put '[JSON] () :<|>
      AuthProtect "cookie-auth" :> Capture "worksDir" T.Text :> Delete '[JSON] () ))

klaraWorksApi :: Proxy KlaraWorksApi
klaraWorksApi = Proxy
