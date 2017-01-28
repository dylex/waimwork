-- |Parsers and generators for HTTP header data.
module Waimwork.HTTP
  ( encodePathSegments'
  , encodePath'
  , splitHTTP
  , quoteHTTP
  , unquoteHTTP
  , formatHTTPDate
  , parseHTTPDate
  ) where

import           Control.Applicative ((<**>), (<|>))
import           Control.Monad (msum)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Format (FormatTime, ParseTime, formatTime, parseTimeM, defaultTimeLocale)
import           Network.HTTP.Types (Query, encodePathSegments, renderQueryBuilder)

-- |Same as 'encodePathSegments', but generates absolute paths (empty results in a single slash)
encodePathSegments' :: [T.Text] -> BSB.Builder
encodePathSegments' [] = BSB.char8 '/'
encodePathSegments' p = encodePathSegments p

-- |Same as 'encodePath' but using 'encodePathSegments''
encodePath' :: [T.Text] -> Query -> BSB.Builder
encodePath' p [] = encodePathSegments' p
encodePath' p q = encodePathSegments' p <> renderQueryBuilder True q

ctls, separators :: String
ctls = "\0-\31\DEL"
separators = "()<>@,;:\\\"/[]?={} \t"

quotedString :: AP.Parser BS.ByteString
quotedString = APC.char '"' *> qdtext <* APC.char '"' where
  qdtext = APC.takeTill (\c -> c == '"' || c == '\\')
    <**> AP.option id (flip BS.append <$> qpair)
  qpair = BS.cons <$> (APC.char '\\' *> AP.anyWord8) <*> qdtext

token :: AP.Parser BS.ByteString
token = AP.takeWhile1 $ AP.notInClass $ separators ++ ctls 

element :: AP.Parser BS.ByteString
element = quotedString <|> token

list :: AP.Parser BS.ByteString -> AP.Parser [BS.ByteString]
list p = filter (not . BS.null)
  <$> AP.sepBy (AP.option BS.empty p) (APC.skipSpace *> APC.char ',' <* APC.skipSpace)
  <* AP.endOfInput

-- |Parse a comma-separated list of quoted string or unquoted tokens
splitHTTP :: BS.ByteString -> Maybe [BS.ByteString]
splitHTTP = either (const Nothing) Just . AP.parseOnly (list element)

-- |Attempt to parse a quoted string, otherwise just take the input as-is.
unquoteHTTP :: BS.ByteString -> BS.ByteString
unquoteHTTP s = either (const s) id $ AP.parseOnly (APC.skipSpace *> element <* APC.skipSpace) s

-- |Quote a string.
quoteHTTP :: BS.ByteString -> BS.ByteString
quoteHTTP = BSC.pack . ('"':) . quote . BSC.unpack where
  quote "" = "\""
  quote ('\\':r) = '\\':'\\':quote r
  quote ('"':r) = '\\':'"':quote r
  quote (c:r)
    | APC.inClass ctls c = '\\':c:quote r
    | otherwise = c:quote r

dateFmts :: [String]
-- rfc1123Date, rfc850Date, asctimeDate
dateFmts = ["%a, %d %b %Y %T GMT", "%A, %d-%b-%y %T GMT", "%a %b %e %T %Y"]

defaultDateFmt :: String
defaultDateFmt = head dateFmts

-- |Render an HTTP date string in rfc1128 format.
formatHTTPDate :: FormatTime t => t -> BS.ByteString
formatHTTPDate = BSC.pack . formatTime defaultTimeLocale defaultDateFmt

-- |Parse an HTTP date string in any valid format: rfc1123, rfc850, asctime
parseHTTPDate :: ParseTime t => BS.ByteString -> Maybe t
parseHTTPDate b = msum $ map (\f -> parseTimeM True defaultTimeLocale f s) dateFmts where s = BSC.unpack b
