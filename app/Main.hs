{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.Text as T
import           Data.Time.Calendar(fromGregorian)

import           Network.Wai.Handler.Warp
import           Network.Wai.Logger(withStdoutLogger)
import           Database.Persist.Sqlite
import           Servant

import           App
import           Api
import           Utils

main :: IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let port = 8080
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSql $ do
          runMigration migrateAll
          insert $ Works "20160813-theseus" "テセウスの私に彼女を愛せるか" (fromGregorian 2016 08 13) "コミックマーケット90" "image-multi" "艦隊これくしょん" True ["11.jpg","14.jpg","17.jpg","38.jpg"] "在庫なし"
        runSettings settings =<< klaraWorksApp
        
