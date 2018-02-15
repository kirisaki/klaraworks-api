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
  ( "works" :> Capture "language" T.Text :>
    ( Get '[JSON] [ApiWorksHeader] :<|>
      Capture "worksDir" T.Text :> Get '[JSON] ApiWorks
    )
  ) :<|>
  AuthProtect "cookie-auth" :>
  (
    "info" :>
    (
      Get '[JSON] [ApiInfo] :<|>
      Capture "infoDir" T.Text :> Get '[JSON] ApiInfo :<|>
      ReqBody '[JSON] ApiInfo :> Post '[JSON] () :<|>
      Capture "infoDir" T.Text :> ReqBody '[JSON] ApiInfo :> Put '[JSON] () :<|>
      Capture "infoDir" T.Text :> Delete '[JSON] ()
    ) :<|>
    "detail" :>
    (
      Get '[JSON] [ApiDetail] :<|>
      Capture "detailDir" T.Text :> Get '[JSON] ApiDetail :<|>
      ReqBody '[JSON] ApiDetail :> Post '[JSON] () :<|>
      Capture "detailDir" T.Text :> ReqBody '[JSON] ApiDetail :> Put '[JSON] () :<|>
      Capture "detailDir" T.Text :> Delete '[JSON] ()
    )
  )

klaraWorksApi :: Proxy KlaraWorksApi
klaraWorksApi = Proxy
