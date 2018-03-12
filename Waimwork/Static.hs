module Waimwork.Static
  ( staticFileResponse
  ) where

import Control.Exception (handleJust)
import Control.Monad (guard)
import Network.HTTP.Types.Header (ResponseHeaders, hIfModifiedSince, hLastModified)
import Network.HTTP.Types.Status (ok200, notModified304, notFound404)
import Network.Wai (Request, requestHeaders, Response, responseBuilder, responseFile)
import System.Directory (doesFileExist, getModificationTime)
import System.IO.Error (isDoesNotExistError)

import Waimwork.HTTP (parseHTTPDate, formatHTTPDate)

-- |Serve a static file by path, returning 304 depending on modification time, and adding a last-modified header.
-- Note that warp now does this itself.
staticFileResponse :: Request -> ResponseHeaders -> FilePath -> IO Response
staticFileResponse req headers path =
  maybe
    (responseBuilder notFound404 [] mempty)
    (\mtime -> do
      if any (mtime <=) $ parseHTTPDate =<< lookup hIfModifiedSince (requestHeaders req)
        then responseBuilder notModified304 [] mempty
        else responseFile ok200 ((hLastModified, formatHTTPDate mtime) : headers) path Nothing)
    <$> handleJust (guard . isDoesNotExistError) (\() -> return Nothing) (do
      exist <- doesFileExist path
      if exist then Just <$> getModificationTime path else return Nothing)
