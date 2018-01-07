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
import           Data.Time
import           GHC.Generics

import           Data.Aeson
import           Database.Persist
import           Database.Persist.TH
import           Servant.API
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
|]

data ApiWorks = ApiWorks 
  { apiWorksDir :: T.Text
  , apiWorksTitle :: T.Text
  , apiWorksDate :: Day
  , apiWorksEvent :: T.Text
  , apiWorksType :: T.Text
  , apiWorksOrigin :: T.Text
  , apiWorksFanart :: Bool
  , apiWorksContents :: [T.Text]
  , apiWorksStatus :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ApiWorks
instance FromJSON ApiWorks
instance ElmType ApiWorks

type KlaraWorksApi =
  "_api" :>
  ("works" :> Get '[JSON] [ApiWorks] :<|>
   "works" :> Capture "apiWorksDir" T.Text :> Get '[JSON] ApiWorks)

klaraWorksApi :: Proxy KlaraWorksApi
klaraWorksApi = Proxy
