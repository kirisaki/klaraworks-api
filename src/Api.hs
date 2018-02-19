{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
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

import           Control.Lens hiding ((:>))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Extensible
import           Database.Persist
import           Database.Persist.TH
import           Servant.API
import           Servant.API.Experimental.Auth
import           Servant.Elm(ElmType)

import           Types

type KlaraWorksApi =
  "_api" :>
  (
    "works" :> Capture "language" T.Text :> Get '[JSON] [ApiWorksHeader]  :<|>
    "works" :> Capture "language" T.Text :> Capture "worksDir" T.Text :> Get '[JSON] ApiWorks :<|>
    "login" :> ReqBody '[JSON] ApiLogin :> Post '[JSON] () :<|>
    "login" :> ReqBody '[JSON] ApiLogin :> Delete '[JSON] () :<|>
    "info" :>  AuthProtect "cookie-auth" :> Get '[JSON] [ApiInfo]  :<|>
    "info" :>  AuthProtect "cookie-auth" :>  Capture "infoDir" T.Text :> Get '[JSON] ApiInfo :<|>
    "info" :>  AuthProtect "cookie-auth" :>  ReqBody '[JSON] ApiInfo :> Post '[JSON] () :<|>
    "info" :>  AuthProtect "cookie-auth" :>  Capture "infoDir" T.Text :> ReqBody '[JSON] ApiInfo :> Put '[JSON] () :<|>
    "info" :>  AuthProtect "cookie-auth" :>  Capture "infoDir" T.Text :> Delete '[JSON] () :<|>
    "detail" :> AuthProtect "cookie-auth" :> Get '[JSON] [ApiDetail] :<|>
    "detail" :> AuthProtect "cookie-auth" :> Capture "detailDir" T.Text :> Get '[JSON] ApiDetail :<|>
    "detail" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] ApiDetail :> Post '[JSON] () :<|>
    "detail" :> AuthProtect "cookie-auth" :> Capture "detailDir" T.Text :> ReqBody '[JSON] ApiDetail :> Put '[JSON] () :<|>
    "detail" :> AuthProtect "cookie-auth" :> Capture "detailDir" T.Text :> Delete '[JSON] ()
    
  )

klaraWorksApi :: Proxy KlaraWorksApi
klaraWorksApi = Proxy
