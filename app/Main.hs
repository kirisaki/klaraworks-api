{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger(withStdoutLogger)
import Servant

import App

main :: IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let port = 8080
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings =<< paprikaApp
        
