{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Proxy
import qualified Data.Text as T
import           GHC.Generics

import           Data.Aeson
import           Database.Persist
import           Database.Persist.TH
import           Servant.API
import           Servant.Elm(ElmType)

type PaprikaApi =
  "_api" :>
  ("article" :> Get '[JSON] [Article] :<|>
   "article" :> Capture "articleName" T.Text :> Get '[JSON] Article)

paprikaApi :: Proxy PaprikaApi
paprikaApi = Proxy

data Article
  = Article {
    articleName :: T.Text,
    articleContent :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ElmType Article
instance ToJSON Article
instance FromJSON Article

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Articledb
        name String
        content T.Text
|]
