{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import           Data.Monoid
import           Data.Proxy
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           GHC.TypeLits

import           Control.Lens hiding ((:>))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Constraint
import           Data.Extensible
import qualified Data.HashMap.Strict as HM
import           Database.Persist
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Info
        dir T.Text
        date Day
        worksType T.Text
        fanart Bool
        UniqueDir dir
        deriving Show Eq
       
    Detail
        dir T.Text
        lang T.Text
        title T.Text
        event T.Text
        origin T.Text
        contents [T.Text]
        status T.Text
        text T.Text
        deriving Show Eq
|]

mkField "dir date worksType fanart lang title event origin contents status text"
  
type ApiInfo = Record
  [ "dir" :> T.Text
  , "date" :> Day
  , "worksType" :> T.Text
  , "fanart" :> Bool
  ]

type ApiDetail = Record
  [ "dir" :> T.Text
  , "lang" :> T.Text
  , "title" :> T.Text
  , "event" :> T.Text
  , "origin" :> T.Text
  , "contents" :> [T.Text]
  , "status" :> T.Text
  , "text" :> T.Text
  ]

type ApiWorksHeader = Record
  [ "dir" :> T.Text
  , "title" :> T.Text
  , "date" :> Day
  ]
  
type ApiWorks = Record
  [ "dir" :> T.Text
  , "title" :> T.Text
  , "date" :> Day
  , "event" :> Maybe T.Text
  , "worksType" :> T.Text
  , "origin" :> Maybe T.Text
  , "fanart" :> Bool
  , "contents" :> [T.Text]
  , "status" :> Maybe T.Text
  , "text" :> Maybe T.Text
  ]


instance Forall (Data.Extensible.KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $ \v -> hgenerateFor (Proxy :: Proxy (Data.Extensible.KeyValue KnownSymbol FromJSON))
    $ \m -> let k = symbolVal (proxyAssocKey m) in case v ^? ix (fromString k) of
      Just a -> Field . pure <$> parseJSON a
      Nothing -> fail $ "Missing key: " ++ k

instance Forall (Data.Extensible.KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
  toJSON rec = Object $ HM.fromList $ flip appEndo [] $ hfoldMap getConst'
               $ hzipWith (\(Comp Dict) v -> Const' $ Endo
                            ((fromString $ symbolVal $ proxyAssocKey v, toJSON $ getField v):))
               (library :: Comp Dict (Data.Extensible.KeyValue KnownSymbol ToJSON) :* xs) rec
