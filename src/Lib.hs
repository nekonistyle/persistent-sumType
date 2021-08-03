{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( startApp
    ) where

import Control.Monad.Logger
import Control.Monad.Reader (ReaderT,liftIO)
import Data.String.Conversions (cs)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import MyPersistField

dbPath :: FilePath
dbPath = "sqlite.db"


type MySum = Either Int String
type MySum3 = MySumType Int Int String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MyData
  value MySum
  deriving Show
MyData3
  value MySum3
  deriving Show
|]

startApp :: IO ()
startApp = do
  pool <- readyPool dbPath migrateAll
  runSqlPool dbOperation3 pool


dbOperation :: MyDatabaseIO ()
dbOperation = do
  leftDataId <- insert $ MyData $ Left 4
  rightDataId <- insert $ MyData $ Right "hello"
  leftData <- get leftDataId
  rightData <- get rightDataId
  liftIO $ print leftData
  liftIO $ print rightData

-- Just (MyData {myDataValue = Left 4})
-- Just (MyData {myDataValue = Right "hello"})


dbOperation3 :: MyDatabaseIO ()
dbOperation3 = do
  dataId1 <- insert $ MyData3 Constructor1
  dataId2 <- insert $ MyData3 $ Constructor2 4
  dataId3 <- insert $ MyData3 $ Constructor3 7 "world"
  data1 <- get dataId1
  data2 <- get dataId2
  data3 <- get dataId3
  liftIO $ print data1
  liftIO $ print data2
  liftIO $ print data3

-- Just (MyData3 {myDataValue = Constructor1})
-- Just (MyData3 {myDataValue = Constructor2 4})
-- Just (MyData3 {myDataValue = Constructor3 7 "world"})


type MyDatabaseIO a = ReaderT SqlBackend IO a


-- ConnectionPool

poolSize :: Int
poolSize = 5

readyPool :: FilePath -> Migration -> IO ConnectionPool
readyPool dbPath migration = do
  pool <- mkPool dbPath
  runSqlPool (runMigration migration) pool
  return pool

mkPool :: FilePath -> IO ConnectionPool
mkPool filePath =
  runStdoutLoggingT $ createSqlitePool (cs filePath) poolSize


