-- |An abort-like way to interrupt a wai application with an early result, in IO.
{-# LANGUAGE FlexibleContexts #-}
module Waimwork.Result
  ( Application
  , result
  , unsafeResult
  , runResult
  , resultApplication
  , routeResultApplicationError
  , routeResultApplication
  , resultMiddleware
  ) where

import Control.Exception.Lifted (Exception, throwIO, throw, handle)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Network.Wai (Request, Response, ResponseReceived, responseStatus, responseBuilder)
import Web.Route.Invertible (RouteMap)
import Web.Route.Invertible.Wai (routeWaiError)

-- |A 'Network.Wai.Application' in an arbitrary monad
type Application m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

newtype Result = Result { resultResponse :: Response } deriving (Typeable)
instance Show Result where
  showsPrec p (Result r) = showParen (p > 10)
    $ showString "Result " . showsPrec 11 (responseStatus r)
instance Exception Result

-- |Generate a result, aborting the rest of the computation.
-- Should only be called from within 'runResult' or 'resultMiddleware'.
result :: MonadBase IO m => Response -> m a
result = throwIO . Result

-- |Generate a 'result' in pure code unsafely: it will only be used if evaluated.
unsafeResult :: Response -> a
unsafeResult = throw . Result

-- |Run a computation, catching any 'result' calls.
runResult :: MonadBaseControl IO m => m Response -> m Response
runResult = handle (return . resultResponse)

-- |Convert a simple function to an 'Application' using 'runResult'.
resultApplication :: MonadBaseControl IO m => (Request -> m Response) -> Application m
resultApplication f q r = liftBase . r =<< runResult (f q)

-- |Combine 'Web.Route.Invertible.Wai.routeWaiError' with 'resultApplication'
routeResultApplicationError :: MonadBaseControl IO m => (Status -> ResponseHeaders -> Request -> m Response) -> RouteMap (Request -> m Response) -> Application m
routeResultApplicationError e m = resultApplication $ routeWaiError e m

-- |Combine 'Web.Route.Invertible.Wai.routeWaiApplication' with 'resultApplication'
routeResultApplication :: MonadBaseControl IO m => RouteMap (Request -> m Response) -> Application m
routeResultApplication = routeResultApplicationError $ \s h _ -> return $ responseBuilder s h mempty

-- |Catch and send any 'result' generated from the application (regardless of whether a response has already been sent).
resultMiddleware :: MonadBaseControl IO m => Application m -> Application m
resultMiddleware app req send =
  handle
    (liftBase . send . resultResponse)
    (app req send)
