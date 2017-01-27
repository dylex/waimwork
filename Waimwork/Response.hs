{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- |Convenient ways to construct 'Response' objects from various data types.
module Waimwork.Response
  ( ResponseData(..)
  , okResponse
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Types (ResponseHeaders, Status, ok200, hContentType)
import Network.Wai (Response, responseBuilder, responseLBS, StreamingBody, responseStream, FilePart(..), responseFile)
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

-- |Data types from which we can construct 'Response' objects.
-- Instances add appropriate headers when relevant (e.g. content-type) and assume utf-8.
class ResponseData r where
  response :: Status -> ResponseHeaders -> r -> Response

instance ResponseData (Status -> ResponseHeaders -> Response) where
  response s h r = r s h

instance ResponseData () where
  response s h () = responseBuilder s h mempty

instance ResponseData BSB.Builder where
  response = responseBuilder

instance ResponseData BSL.ByteString where
  response = responseLBS

instance ResponseData BS.ByteString where
  response s h = responseBuilder s h . BSB.byteString

instance ResponseData StreamingBody where
  response = responseStream

instance ResponseData ((BSB.Builder -> IO ()) -> IO ()) where
  response s h f = responseStream s h (\w _ -> f w)

instance ResponseData ((BS.ByteString -> IO ()) -> IO ()) where
  response s h f = responseStream s h (\w l -> f (\b -> if BS.null b then l else w (BSB.byteString b)))

instance ResponseData (FilePath, Maybe FilePart) where
  response s h (f, p) = responseFile s h f p

instance ResponseData (FilePath, FilePart) where
  response s h (f, p) = response s h (f, Just p)

instance ResponseData String where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . BSB.stringUtf8

instance ResponseData T.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . TE.encodeUtf8Builder

instance ResponseData TL.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . TLE.encodeUtf8Builder

instance ResponseData JSON.Value where
  response s h =
    response s ((hContentType, "application/json") : h) . JSON.encode

instance ResponseData JSON.Encoding where
  response s h =
    response s ((hContentType, "application/json") : h) . JSON.fromEncoding

instance ResponseData JSON.Series where
  response s h =
    response s h . JSON.pairs

instance ResponseData Html.Html where
  response s h =
    response s ((hContentType, "text/html;charset=utf-8") : h) . Html.renderHtmlBuilder

okResponse :: ResponseData r => ResponseHeaders -> r -> Response
okResponse = response ok200
