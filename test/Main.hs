{-# LANGUAGE OverloadedStrings #-}
import           Crypto.Random.EntropyPool (EntropyPool, createEntropyPool)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Network.HTTP.Types.URI (urlEncode)
import qualified Network.Wai as Wai
import           System.Exit (exitFailure)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Test as Q (isSuccess)
import qualified Text.Blaze.Renderer.Utf8 as BM
import qualified Web.Cookie as Cook

import qualified Blaze.ByteString.Builder.Html.Utf8 as BU
import qualified Blaze.ByteString.Builder.Html.Word as BW
import qualified Waimwork.Blaze as DB
import qualified Text.Blaze as TB
import           Waimwork.Cookie (Secret(..), setSignedCookie, getSignedCookie)
import qualified Waimwork.HTTP as HTTP

assert :: Bool -> IO ()
assert False = exitFailure
assert True = return ()

beq :: B.Builder -> B.Builder -> Q.Property
beq = (Q.===) `on` B.toLazyByteString

meq :: TB.Markup -> TB.Markup -> Q.Property
meq = (Q.===) `on` BM.renderMarkup

checkText :: String -> Q.Property
checkText s = Q.label "blazeText" $
  BU.fromHtmlEscapedText t `beq` BW.fromHtmlEscapedText t
  Q..&&. DB.text t `meq` TB.text t
  where t = T.pack s

checkLazyText :: String -> Q.Property
checkLazyText s = Q.label "blazeLazyText" $
  BU.fromHtmlEscapedLazyText t `beq` BW.fromHtmlEscapedLazyText t
  Q..&&. DB.lazyText t `meq` TB.lazyText t
  where t = TL.pack s

checkCookie :: EntropyPool -> String -> String -> String -> Q.Property
checkCookie rnd ks ns vs = Q.label "cookie" $ Q.ioProperty $ do
  ("set-cookie", c) <- setSignedCookie rnd k Wai.defaultRequest n Nothing v
  let sc = Cook.parseSetCookie c
      v' = getSignedCookie k n Wai.defaultRequest
        { Wai.requestHeaders = [("cOOkIe", BSL.toStrict $ B.toLazyByteString $ Cook.renderCookies [(Cook.setCookieName sc, Cook.setCookieValue sc)])] }
  return $ v' Q.=== Just v
  where
  k = Secret $ BSC.pack ks
  n = urlEncode True $ BSC.pack ns
  v = urlEncode True $ BSC.pack vs

checkHTTP :: Q.Property
checkHTTP = Q.label "HTTP"
  $      HTTP.splitHTTP "  , \"abc\\\"\\\\\",, x" Q.=== Just ["abc\"\\", "x"]
  Q..&&. HTTP.unquoteHTTP "  \" \" " Q.=== " "
  Q..&&. (\s -> let b = BSC.pack s in HTTP.unquoteHTTP (HTTP.quoteHTTP b) Q.=== b)
  Q..&&. (\t -> let d = posixSecondsToUTCTime (fromInteger t) in HTTP.parseHTTPDate (HTTP.formatHTTPDate d) Q.=== Just d)

main :: IO ()
main = do
  rnd <- createEntropyPool
  q <- Q.quickCheckResult
    $      checkText
    Q..&&. checkLazyText
    Q..&&. checkCookie rnd
    Q..&&. checkHTTP
  assert $ Q.isSuccess q
