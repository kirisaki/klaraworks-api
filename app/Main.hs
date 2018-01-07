{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger(withStdoutLogger)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Servant

import           App
import           Api

main :: IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let port = 8080
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSqlite ":memory:" $ do runMigration migrateAll
        runSettings settings =<< klaraWorksApp
        
