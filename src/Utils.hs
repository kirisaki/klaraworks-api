{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Database.Persist.Sqlite

runSql :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runSql = runStdoutLoggingT . runResourceT . withSqliteConn "test.db" . runSqlConn
