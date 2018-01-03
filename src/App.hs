{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.Trans.Except
import Data.List
import qualified Data.Text as T

import Network.Wai
import Servant

import Api

paprikaApp :: IO Application
paprikaApp = return $ serve paprikaApi server

server :: Server PaprikaApi
server =
  getArticleList :<|>
  getArticle

getArticleList :: Handler [Article]
getArticleList = return exampleArticles

getArticle :: T.Text -> Handler Article
getArticle str =
  let
    i = findIndex (\a -> articleName a == str) exampleArticles
  in
    case i of
      Just n -> return $ exampleArticles !! n
      Nothing -> throwError err404

exampleArticles :: [Article]
exampleArticles = [ Article "foo" "example Article"
                  , Article "bar" "nyaaaan"
                  ]
