import qualified Data.ByteString.Builder as B
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Exit (exitFailure)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Test as Q (isSuccess)
import qualified Text.Blaze.Renderer.Utf8 as BM

import qualified Blaze.ByteString.Builder.Html.Utf8 as BU
import qualified Blaze.ByteString.Builder.Html.Word as BW
import qualified Dwebework.Blaze as DB
import qualified Text.Blaze as TB

assert :: Bool -> IO ()
assert False = exitFailure
assert True = return ()

beq :: B.Builder -> B.Builder -> Q.Property
beq = (Q.===) `on` B.toLazyByteString

meq :: TB.Markup -> TB.Markup -> Q.Property
meq = (Q.===) `on` BM.renderMarkup

checkText :: String -> Q.Property
checkText s =
  BU.fromHtmlEscapedText t `beq` BW.fromHtmlEscapedText t
  Q..&&. DB.text t `meq` TB.text t
  where t = T.pack s

checkLazyText :: String -> Q.Property
checkLazyText s =
  BU.fromHtmlEscapedLazyText t `beq` BW.fromHtmlEscapedLazyText t
  Q..&&. DB.lazyText t `meq` TB.lazyText t
  where t = TL.pack s

main :: IO ()
main = do
  q <- Q.quickCheckResult
    $      checkText
    Q..&&. checkLazyText
  assert $ Q.isSuccess q
