-- |Database interface based on postgresql-typed
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Dwebework.Database.PostgreSQL
  (
  -- * Connection management
    configPGDatabase
  , DBPool
  , DBConn
  , initDB
  , finiDB
  , withDB
  -- * Template Haskell interface
  , useTDBConfig
  -- * Convenience wrappers around "Database.PostgreSQL.Typed"
  , pgRunQuery
  , pgExecute
  , pgExecuteSimple
  , pgExecute1
  , pgExecute1'
  , pgExecute_
  , pgQuery
  , pgQuery1
  , pgQuery1'
  , pgTransaction
  ) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool, withResource, createPool, destroyAllResources)
import Database.PostgreSQL.Typed.Protocol (PGDatabase(..), defaultPGDatabase, PGConnection, pgConnect, pgDisconnect, pgSimpleQueries_, pgTransaction)
import Database.PostgreSQL.Typed.Query (PGQuery, PGSimpleQuery, pgRunQuery, pgExecute, pgQuery)
import Database.PostgreSQL.Typed.TH (useTPGDatabase)
import qualified Language.Haskell.TH as TH
import Network (PortID(..))

import qualified Dwebework.Config as C

-- |Create 'PGDatabase' connection information from a configuration section.
-- Uses the following configuration keys and defaults:
--
--   * host [localhost] /or/ sock [\/tmp\/.s.PGSQL.5432]
--   * user
--   * port [5432]
--   * db [user]
--   * pass []
--   * debug [false]
configPGDatabase :: C.Config -> PGDatabase
configPGDatabase conf = defaultPGDatabase
  { pgDBHost = fromMaybe "localhost" host
  , pgDBPort = if isJust host
      then PortNumber (maybe 5432 fromInteger $ conf C.! "port")
      else UnixSocket (fromMaybe "/tmp/.s.PGSQL.5432" $ conf C.! "sock")
  , pgDBName = fromMaybe user $ conf C.! "db"
  , pgDBUser = user
  , pgDBPass = fromMaybe "" $ conf C.! "pass"
  , pgDBDebug = fromMaybe False $ conf C.! "debug"
  }
  where
  host = conf C.! "host"
  user = conf C.! "user"

-- |A 'Pool' of database connections.
newtype DBPool = PGPool { pgPool :: Pool PGConnection }
-- |A single database connection.
type DBConn = PGConnection

-- |Create a new database connection pool from a configuration section.
-- In addition to the keys used by 'configPGDatabase', expects:
--
--   * stripes [1]
--   * idle [300]
--   * maxconn [16]
initDB :: C.Config -> IO DBPool
initDB conf =
  PGPool <$> createPool
    (pgConnect db)
    pgDisconnect
    stripes (fromInteger idle) conn
  where
  db = configPGDatabase conf
  stripes = fromMaybe 1 $ conf C.! "stripes"
  idle = fromMaybe 300 $ conf C.! "idle"
  conn = fromMaybe 16 $ conf C.! "maxconn"

-- |Close all connections in a 'DBPool'.
finiDB :: DBPool -> IO ()
finiDB = destroyAllResources . pgPool

-- |Execute an action that requires a database connection.
-- This could, for example, be an entire request handler.
-- The connection is returned to the pool once the action completes and should not be reused.
withDB :: DBPool -> (DBConn -> IO a) -> IO a
withDB = withResource . pgPool

-- |Call 'useTPGDatabase' using database configuration from a file under the given config key.
useTDBConfig :: FilePath -> C.Path -> TH.DecsQ
useTDBConfig conf key = useTPGDatabase . configPGDatabase . C.get key =<< TH.runIO (C.load conf)

-- |Specialized version of 'pgExecute' for convenient use of the "IsString" instance
pgExecuteSimple :: DBConn -> PGSimpleQuery () -> IO Int
pgExecuteSimple = pgExecute

-- |'dbExecute' a query that expects at most one affected row, applying 'toEnum' to the result or failing
pgExecute1 :: (PGQuery q (), Show q) => DBConn -> q -> IO Bool
pgExecute1 c q = do
  r <- pgExecute c q
  case r of
    0 -> return False
    1 -> return True
    _ -> fail $ "dbExecute1 " ++ show q ++ ": " ++ show r ++ " rows"

-- |'dbExecute1' but fail on false
pgExecute1' :: (PGQuery q (), Show q) => DBConn -> q -> IO ()
pgExecute1' c q = do
  r <- pgExecute1 c q
  unless r $ fail $ "dbExecute1' " ++ show q ++ ": failed"

-- |'pgSimpleQueries_'
pgExecute_ :: DBConn -> BSL.ByteString -> IO ()
pgExecute_ = pgSimpleQueries_

-- |'dbQuery' a query that expects at most one result, or fail
pgQuery1 :: (PGQuery q a, Show q) => DBConn -> q -> IO (Maybe a)
pgQuery1 c q = do
  r <- pgQuery c q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail $ "dbQuery1 " ++ show q ++ ": too many results"

-- |'dbQuery' a query that expects exactly one result, or fail
pgQuery1' :: (PGQuery q a, Show q) => DBConn -> q -> IO a
pgQuery1' c q = maybe (fail $ "dbQuery1' " ++ show q ++ ": no results") return =<< pgQuery1 c q
