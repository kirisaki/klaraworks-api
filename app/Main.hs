{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Data.Text as T
import qualified Data.Time.Calendar

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
      insertUnique testInfo01
      insertUnique testdetail01
      insertUnique testInfo02
      insertUnique testdetail02
      insertUnique testInfo03
      insertUnique testdetail03
    runSettings settings =<< klaraWorksApp
        
testInfo01 :: Info
testInfo01 = Info { infoDir = "20180215-test"
                  , infoDate = Data.Time.Calendar.fromGregorian 2018 2 15
                  , infoWorksType = "manga"
                  , infoFanart = True
                  }

testInfo02 :: Info
testInfo02 = Info { infoDir = "20180215-untitled"
                  , infoDate = Data.Time.Calendar.fromGregorian 2018 2 15
                  , infoWorksType = "manga"
                  , infoFanart = True
                  }

testInfo03 :: Info
testInfo03 = Info { infoDir = "20180216-untitled"
                  , infoDate = Data.Time.Calendar.fromGregorian 2018 2 16
                  , infoWorksType = "image"
                  , infoFanart = True
                  }
        
testdetail01 :: Detail
testdetail01 = Detail { detailDir = "20180215-test"
                      , detailLang = "jpn"
                      , detailTitle = "てすとだよ"
                      , detailEvent = "コミケ"
                      , detailOrigin = "艦これ"
                      , detailContents = ["01.jpg", "02.jpg"]
                      , detailStatus = "在庫あり"
                      , detailText = ""
                      , detailLink = ""
                      }
testdetail02 :: Detail
testdetail02 = Detail { detailDir = "20180215-test"
                      , detailLang = "eng"
                      , detailTitle = "test dayo"
                      , detailEvent = "comike"
                      , detailOrigin = "kancolle"
                      , detailContents = ["01.jpg", "02.jpg"]
                      , detailStatus = "in stock"
                      , detailText = ""
                      , detailLink = ""
                      }
        
testdetail03 :: Detail
testdetail03 = Detail { detailDir = "20180216-untitled"
                      , detailLang = "jpn"
                      , detailTitle = "にゃーん"
                      , detailEvent = "コミケ"
                      , detailOrigin = "艦これ"
                      , detailContents = ["01.jpg", "02.jpg"]
                      , detailStatus = "在庫あり"
                      , detailText = ""
                      , detailLink = ""
                      }
        
