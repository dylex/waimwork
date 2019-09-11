{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |Wrappers around 'Warp.run' that use dynamic "Waimwork.Config" configuration.
module Waimwork.Warp
  ( runWarpSettings
  , runWarp
  , runWaimwork
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
#ifdef VERSION_warp_tls
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
#endif

import qualified Waimwork.Config as C
import Waimwork.Log (Logs, initLogs, accessLogMiddleware, logError)
import Waimwork.Result (resultMiddleware, resultApplication)

-- |Run warp according to the given configuration settings.
-- Supports the following keys for warp settings:
--
--   [@port@] 'Warp.setPort' (/required/)
--   [@host@] 'Warp.setHost'
--   [@timeout@] 'Warp.setTimeout'
--   [@fdcache@] 'Warp.setFdCacheDuration'
--   [@fileinfocache@] 'Warp.setFileInfoCacheDuration'
--   [@servername@] 'Warp.setServerName'
--   [@slowloris@] 'Warp.setSlowlorisSize'
--   [@gracefulshutdown@] 'Warp.setGracefulShutdownTimeout'
--   [@http2@] @'Warp.setHTTP2Disabled' . 'not'@
--   [@ssl.key@] File containing the SSL key. If both this and @ssl.cert@ are specified, https is enabled.
--   [@ssl.cert@] File containing the SSL certificate or array of certificate and chain files
--   [@ssl.insecure@] 'WarpTLS.onInsecure' boolean to allow plain-text http connections or string error to return for attempts
--
-- Any configuration settings override those specified in 'Warp.Settings'.
-- Uses 'resultMiddleware', 'accessLogMiddleware' and 'logError' to generate logs.
runWarpSettings :: Warp.Settings -> C.Config -> Logs -> Wai.Application -> IO ()
runWarpSettings set conf logs app =
  run                                         (conf C.! "ssl.key")
                         (either id return <$> conf C.! "ssl.cert")
    ( Warp.setPort                            (conf C.! "port")
    $ maybe id (Warp.setHost . fromString)    (conf C.! "host")
    $ maybe id Warp.setTimeout                (conf C.! "timeout")
#if MIN_VERSION_warp(3,0,13)
    $ maybe id Warp.setFdCacheDuration        (conf C.! "fdcache")
#endif
    $ maybe id Warp.setFileInfoCacheDuration  (conf C.! "fileinfocache")
#if MIN_VERSION_warp(3,0,2)
    $ maybe id Warp.setServerName             (conf C.! "servername")
#endif
#if MIN_VERSION_warp(3,1,2)
    $ maybe id Warp.setSlowlorisSize          (conf C.! "slowloris")
#endif
#if MIN_VERSION_warp(3,2,8)
    $ Warp.setGracefulShutdownTimeout         (conf C.! "gracefulshutdown")
#endif
#if MIN_VERSION_warp(3,1,7)
    $ (if and (conf C.! "http2" :: Maybe Bool) then id else Warp.setHTTP2Disabled)
#endif
    $ Warp.setOnException (logError logs)
    set) $
    accessLogMiddleware logs $ resultMiddleware app
  where
#ifdef VERSION_warp_tls
  run (Just k) (Just (cert:chain)) = WarpTLS.runTLS
    (WarpTLS.tlsSettingsChain cert chain k)
    { WarpTLS.onInsecure = case conf C.! "ssl.insecure" of
       Nothing -> WarpTLS.onInsecure WarpTLS.defaultTlsSettings 
       Just (Left False) -> WarpTLS.DenyInsecure "This server only accepts secure HTTPS connections."
       Just (Left True) -> WarpTLS.AllowInsecure
       Just (Right s) -> WarpTLS.DenyInsecure (BSL.fromStrict s)
    }
#endif
  run _ _ = Warp.runSettings

-- |@'runWarpSettings' 'Warp.defaultSettings'@
runWarp :: C.Config -> Logs -> Wai.Application -> IO ()
runWarp = runWarpSettings Warp.defaultSettings

-- |Use \"log\" and \"http\" sections of the config file for 'runWarp'.
runWaimwork :: C.Config -> (Wai.Request -> IO Wai.Response) -> IO ()
runWaimwork conf app = do
  logs <- initLogs (conf C.! "log")
  -- we already have a resultMiddleware above, but I feel like we shouldn't... regardless, it's safe to catch here first
  runWarp (conf C.! "http") logs (resultApplication app)
