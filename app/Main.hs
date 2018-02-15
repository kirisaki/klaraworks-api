{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.Text as T
import qualified Data.Time as Time

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger(withStdoutLogger)
import           Database.Persist.Sqlite
import           Servant

import           App
import           Api
import           Utils
import           Types

main :: IO ()
main = do
  withStdoutLogger $ \aplogger -> do
        let port = 8080
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSql $ do
          runMigration migrateAll
        runSettings settings =<< klaraWorksApp

        
