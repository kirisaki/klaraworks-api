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
  , event :: T.Text
  , worksType :: T.Text
  , origin :: T.Text
  , fanart :: Bool
  , contents :: [T.Text]
  , status :: T.Text
  }
  deriving (Eq, Show, Generic)

data ApiWorksReq = ApiWorksReq 
  { reqDir :: T.Text
  , reqTitle :: T.Text
  , reqDate :: Day
  , reqEvent :: Maybe T.Text
  , reqWorksType :: T.Text
  , reqOrigin :: Maybe T.Text
  , reqFanart :: Bool
  , reqContents :: [T.Text]
  , reqStatus :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)
deriveFromJSON defaultOptions ''ApiWorksReq
    
modelToApiWorks :: Works -> ApiWorks
modelToApiWorks w = ApiWorks
  { dir = worksDir w
  , title = worksTitle w
  , date = worksDate w
  , event = worksEvent w
  , worksType = worksWorksType w
  , origin = worksOrigin w
  , fanart = worksFanart w
  , contents = worksContents w
  , status = worksStatus w
  }

reqToModel :: ApiWorksReq -> Works
reqToModel req = let
  emptyString s =
    case s of
      Just str -> str
      Nothing -> ""
  in
    Works
    { worksDir = reqDir req
    , worksTitle = reqTitle req
    , worksDate = reqDate req
    , worksEvent = emptyString $ reqEvent req
    , worksWorksType = reqWorksType req
    , worksOrigin = emptyString $ reqOrigin req
    , worksFanart =  reqFanart req
    , worksContents = reqContents req
    , worksStatus = emptyString $ reqStatus req
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
      Capture "apiWorksDir" T.Text :> Get '[JSON] ApiWorks :<|>
      ReqBody '[JSON] ApiWorksReq :> Post '[JSON] () ))

klaraWorksApi :: Proxy KlaraWorksApi
klaraWorksApi = Proxy
