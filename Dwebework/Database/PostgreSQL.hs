-- |Database interface based on postgresql-typed
{-# LANGUAGE OverloadedStrings #-}
module Dwebework.Database.PostgreSQL
  ( configPGDatabase
  , DBPool
  , DBConn
  , initDB
  , finiDB
  , withDB
  ) where

import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool, withResource, createPool, destroyAllResources)
import Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase, PGConnection, pgConnect, pgDisconnect)
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

