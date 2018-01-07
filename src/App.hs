{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.List
import           Data.Time.Calendar(fromGregorian)
import qualified Data.Text as T

import           Network.Wai
import           Database.Persist
import           Servant

import           Api

klaraWorksApp :: IO Application
klaraWorksApp = return $ serve klaraWorksApi server

server :: Server KlaraWorksApi
server =
  getWorksList :<|>
  getWorks

getWorksList :: Handler [ApiWorks]
getWorksList = return exampleWorks

getWorks :: T.Text -> Handler ApiWorks
getWorks str =
  let
    i = findIndex (\a ->  apiWorksDir a == str) exampleWorks
  in
    case i of
      Just n -> return $ exampleWorks !! n
      Nothing -> throwError err404

exampleWorks :: [ApiWorks]
exampleWorks = [ ApiWorks "20160813-theseus" "テセウスの私に彼女を愛せるか" (fromGregorian 2016 08 13) "コミックマーケット90" "image-multi" "艦隊これくしょん" True ["11.jpg","14.jpg","17.jpg","38.jpg"] "在庫なし"
                  ]
