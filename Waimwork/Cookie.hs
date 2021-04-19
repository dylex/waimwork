{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Cryptographically signed cookies.
-- Using a sufficiently long secret key on the server, this can prevent cookies from being tampered with by the client.  It does not, prevent users (or others snooping on cookies) from seeing the value in the cookie, just from channging it.  Sensitive or authentication data should probably be wrapped in an additional layer of indirection.
module Waimwork.Cookie
  ( Secret(..)
  , initSecret
  , setSignedCookie
  , getSignedCookie
  , clearCookie
  ) where

import Control.Monad (guard)
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Crypto.Random.EntropyPool (EntropyPool, getEntropyFrom)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base64URLUnpadded))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types.Header (Header, hCookie)
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cook

import qualified Waimwork.Config as C

-- |A persistant random byte sequence which should be kept private.
newtype Secret = Secret{ secretKey :: BS.ByteString }
  deriving (C.Configurable, BA.ByteArrayAccess)

-- |Load a secret from the config (just @'C.get' \"secret\"@)
initSecret :: C.Config -> Secret
initSecret = C.get "secret"

encode :: BA.ByteArrayAccess a => a -> BS.ByteString
encode = convertToBase Base64URLUnpadded

decode :: BS.ByteString -> Maybe BS.ByteString
decode = either (const Nothing) Just . convertFromBase Base64URLUnpadded

hmac :: Secret -> BS.ByteString -> HMAC.HMAC Hash.Skein256_224
hmac = HMAC.hmac

hmac0 :: HMAC.HMAC Hash.Skein256_224
hmac0 = hmac (Secret "") ""

hmacBytes, hmacLength, nonceBytes, nonceLength :: Int
hmacBytes = BS.length $ BA.convert hmac0
hmacLength = BS.length $ encode hmac0
nonceBytes = 6
nonceLength = BS.length $ encode $ (BA.zero nonceBytes :: BS.ByteString) -- 8

sign :: EntropyPool -> Secret -> BS.ByteString -> IO BS.ByteString
sign rnd key msg = do
  nonce <- getEntropyFrom rnd nonceBytes
  return $ encode (BA.convert (hmac key (msg <> nonce)) <> nonce) <> msg

unSign :: Secret -> BS.ByteString -> Maybe BS.ByteString
unSign key sigmsg = do
  let (signonce, msg) = BS.splitAt (hmacLength + nonceLength) sigmsg
  (sig, nonce) <- BS.splitAt hmacBytes <$> decode signonce
  guard $ BA.constEq sig $ hmac key (msg <> nonce)
  return msg

-- |Generate a header to set a cookie.
setCookie :: Cook.SetCookie -> Header
setCookie sc = ("set-cookie", BSL.toStrict $ BSB.toLazyByteString $ Cook.renderSetCookie sc)

-- |Generate a header to set a cookie with the given name, expiration time, and value, signing (but not obscuring) the value with the given secret first.
-- 'getSignedCookie' should return the same value if given the same secret.
setSignedCookie :: EntropyPool -> Secret -> Wai.Request -> BS.ByteString -> Maybe UTCTime -> BS.ByteString -> IO Header
setSignedCookie rnd key req name ex val = do
  val' <- sign rnd key val
  return $ setCookie Cook.def
    { Cook.setCookieName = name
    , Cook.setCookieValue = val'
    , Cook.setCookiePath = Just "/"
    , Cook.setCookieExpires = ex
    , Cook.setCookieSecure = Wai.isSecure req
    , Cook.setCookieHttpOnly = True
    }

-- |Retrieve a cookie by name and verify its signature against the given secret.
getSignedCookie :: Secret -> BS.ByteString -> Wai.Request -> Maybe BS.ByteString
getSignedCookie key name req =
  unSign key =<< lookup name . Cook.parseCookies =<< lookup hCookie (Wai.requestHeaders req)

-- |Generate a header to clear the cookie with the given name.
clearCookie :: BS.ByteString -> Header
clearCookie c = setCookie Cook.def
  { Cook.setCookieName = c
  , Cook.setCookiePath = Just "/"
  }
