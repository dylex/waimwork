{-# LANGUAGE OverloadedStrings #-}
-- |A simple logging system for web servers.
-- Provides a wrapper around fast-logger for two log files, access and messages, as well as a way to write simple apache-style access logs.
module Waimwork.Log
  ( Logs
  , initLogs
  , finiLogs
  , LogStr
  , toLogStr
  , requestLog
  , logMsg
  , logAccess
  , logError
  , accessLogMiddleware
  ) where

import Control.Exception (Exception, SomeException(..), finally)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Log.FastLogger

import qualified Waimwork.Config as C

-- |Handles to log files.
data Logs = Logs
  { loggerMessages, loggerAccess :: Maybe LoggerSet
  }

initLog :: FilePath -> C.Config -> IO (Maybe LoggerSet)
initLog def conf = do
  case file of
    "" -> return Nothing
    "stdout" -> Just <$> newStdoutLoggerSet buf
    "stderr" -> Just <$> newStderrLoggerSet buf
    _ -> do
      check file
      mapM_ (rotate . FileLogSpec file size) (num :: Maybe Int)
      Just <$> newFileLoggerSet buf file
  where
  file = fromMaybe def $ conf C.! "file"
  buf = fromMaybe defaultBufSize $ conf C.! "buf"
  num = conf C.! "rotate"
  size = fromMaybe (1024*1024) $ conf C.! "size"

-- |Open the log files account to the config.
-- The config expects the following settings.
--
-- For 'logMsg':
--
-- > messages {
-- >   # File to log to, "stderr", "stdout", or "" to disable
-- >   file = "stderr"
-- >   # Buffer size
-- >   buf = 4096
-- >   # Rotate log files and keep this many
-- >   #rotate = COUNT 
-- >   # File size at which to rotate
-- >   size = 1048576
-- > }
--
-- For 'logAccess':
--
-- > access {
-- >   file = "stdout"
-- >   # same as for messages above
-- > }
initLogs :: C.Config -> IO Logs
initLogs conf = Logs
  <$> initLog "stderr" (conf C.! "messages")
  <*> initLog "stdout" (conf C.! "access")

-- |Flush and close log files.
finiLogs :: Logs -> IO ()
finiLogs (Logs lm la) =
  mapM_ flushLogStr $ catMaybes [lm, la]

str :: ToLogStr a => a -> LogStr
str = toLogStr

char :: Char -> LogStr
char = str . BSC.singleton

pad :: ToLogStr a => Int -> a -> LogStr
pad n m
  | n < 0 = s <> p
  | otherwise = p <> s
  where
  s = str m
  p = str $ BSC.replicate (abs n - logStrLength s) ' '

quote :: Show a => Maybe a -> LogStr
quote = maybe (char '-') (str . show) -- FIXME, inefficient

time :: ZonedTime -> LogStr
time = str . formatTime defaultTimeLocale "%F %X"

infixr 6 &
(&) :: LogStr -> LogStr -> LogStr
x & y = x <> char ' ' <> y

logStr :: LoggerSet -> UTCTime -> LogStr -> IO ()
logStr l t s = do
  zt <- utcToLocalZonedTime t
  pushLogStr l $ time zt & s <> char '\n'

-- |Produce a 'LogStr' for a request access
-- Given the incoming request time, request, response time, response, the output is of the form:
--
-- > YYYY-MM-DD HH:MM:SS CLIENT.IP       STATUS RT METHOD PATH?QUERY REDIRECT-LOCATION "REFERER" "USER-AGENT"
--
-- Where RT is the response time minus the request time in miliseconds.
requestLog :: UTCTime -> Wai.Request -> UTCTime -> Wai.Response -> IO LogStr
requestLog qt q rt r = do
  (Just h, Nothing) <- Net.getNameInfo [Net.NI_NUMERICHOST] True False $ Wai.remoteHost q
  return
    $ pad (-15) h
    & pad 3 (show $ HTTP.statusCode $ Wai.responseStatus r)
    -- & pad 4 (fromMaybe "-" u)
    & pad 4 (show (floor $ 1000 * rt `diffUTCTime` qt :: Integer))
    & str (Wai.requestMethod q)
    & str (Wai.rawPathInfo q) <> str (Wai.rawQueryString q)
    & quote (lookup "location" rh)
    & quote (lookup "referer" qh)
    & quote (lookup "user-agent" qh)
  where
  qh = Wai.requestHeaders q
  rh = Wai.responseHeaders r

-- |Log a message to the messages log, appending a newline.
logMsg :: ToLogStr a => Logs -> UTCTime -> a -> IO ()
logMsg Logs{ loggerMessages = Just l } t m = do
  logStr l t $ str m
logMsg _ _ _ = return ()

-- |Log a request to the access log using 'requestLog', appending a newline.
logAccess :: Logs -> UTCTime -> Wai.Request -> Wai.Response -> IO ()
logAccess Logs{ loggerAccess = Just l } qt q r = do
  rt <- getCurrentTime
  logStr l qt =<< requestLog qt q rt r
logAccess _ _ _ _ = return ()

-- |Log an exception to the messages log, possibly associated with a request.
-- This is sutable for use with 'Warp.setOnException'.
logError :: Exception e => Logs -> Maybe Wai.Request -> e -> IO ()
logError Logs{ loggerMessages = Just l } mq e = do
  t <- getCurrentTime
  msg <- mapM (\q -> requestLog t q t $ Warp.exceptionResponseForDebug $ SomeException e) mq
  logStr l t $ maybe id ((<>) . (<> "\n")) msg $ toLogStr $ show e
logError _ _ _ = return ()

-- |Log every request once the response is sent.
accessLogMiddleware :: Logs -> Wai.Middleware
accessLogMiddleware Logs{ loggerAccess = Just l } a = \q s -> do
  qt <- getCurrentTime
  a q $ \r -> finally (s r) $ do
    rt <- getCurrentTime
    logStr l qt =<< requestLog qt q rt r
accessLogMiddleware _ a = a

