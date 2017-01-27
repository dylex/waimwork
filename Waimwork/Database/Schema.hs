-- |Simple forward-only schema migration management
{-# LANGUAGE OverloadedStrings #-}
module Waimwork.Database.Schema
  ( updateDBSchema
  ) where

import Control.Arrow (first, second)
import Control.Exception (tryJust)
import Control.Monad (guard, (<=<))
import Data.List (sort)
import qualified Database.HDBC as HDBC
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), (<.>), splitExtension)
import System.IO (stderr, hPutStr, hPutChar, hFlush)

schemaError :: String -> IO a
schemaError = fail

confirm :: String -> IO ()
confirm s = do
  hPutStr stderr s
  hPutChar stderr ' '
  hFlush stderr
  a <- getLine
  case a of
    ('y':_) -> return ()
    ('Y':_) -> return ()
    _ -> schemaError "Schema update aborted."

sqlFile :: HDBC.IConnection c => c -> FilePath -> IO ()
sqlFile c = HDBC.runRaw c <=< readFile

schemaList :: [FilePath] -> [String]
schemaList l = sort [ n | (n, ".sql") <- map splitExtension l ]

diffs :: [String] -> [String] -> ([String], [String])
diffs x [] = (x, [])
diffs [] y = ([], y)
diffs xa@(x:xl) ya@(y:yl) = case compare x y of
  LT -> first (x :) $ diffs xl ya
  EQ -> diffs xl yl
  GT -> second (y :) $ diffs xa yl

tryDNE :: IO a -> IO (Maybe a)
tryDNE = fmap (either (const Nothing) Just) . tryJust (guard . ("42P01" ==) . HDBC.seState)

-- |Apply all the SQL files in a directory that have not yet been recorded in the schema table.
updateDBSchema :: HDBC.IConnection c => FilePath -> c -> IO ()
updateDBSchema dir dbh = do
  sl <- schemaList <$> getDirectoryContents dir

  lr <- tryDNE $ HDBC.quickQuery dbh "SELECT name FROM schema ORDER BY name" []
  dl <- maybe
    (do
      confirm "No schema found. Initialize?"
      HDBC.rollback dbh
      HDBC.runRaw dbh "CREATE TABLE schema (name varchar(64) Primary Key, applied timestamptz NOT NULL Default now())"
      HDBC.commit dbh
      return [])
    (return . map (HDBC.fromSql . head)) lr

  case diffs sl dl of
    (l, []) -> mapM_ apply l
    (_, e) -> schemaError $ "Inconsistent schema, missing: " ++ unwords (map show e)

  return ()
  where
  file = (dir </>) . (<.> ".sql")
  apply n = do
    confirm $ "Apply schema " ++ show n ++ "?"
    _ <- HDBC.run dbh "INSERT INTO schema (name) VALUES (?)" [HDBC.SqlString n]
    sqlFile dbh (file n)
    HDBC.commit dbh

