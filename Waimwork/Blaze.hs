-- |Additions to "Text.Blaze" for pre-escaped 'BS.ByteString's, 'BSB.Builder's, and "Web.Route.Invertible".
module Waimwork.Blaze
  ( -- * Converting values to markup
    unsafeBuilder
  , preEscapedBuilder
  , builder
  , byteString
  , lazyByteString
  , text
  , lazyText

    -- * Converting values to attributes
  , unsafeBuilderValue
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , textValue
  , lazyTextValue
  , (!?)
  , routeActionValue
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Network.HTTP.Types.URI (Query, encodePathSegments, renderQueryBuilder, simpleQueryToQuery)
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze as B
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.Internal as R

import qualified Blaze.ByteString.Builder.Html.Word as BW

-- |Insert a 'BSB.Builder'. See 'B.unsafeByteString' for caveats.
unsafeBuilder :: BSB.Builder -> B.Markup
unsafeBuilder = B.unsafeLazyByteString . BSB.toLazyByteString

-- |Same as 'B.unsafeLazyByteString' but marked as pre-escaped (e.g., for script tags and others marked external that suppress unescaped content).
preEscapedLazyByteString :: BSL.ByteString -> B.Markup
preEscapedLazyByteString b = Markup.Content (Markup.PreEscaped $ foldMap Markup.ByteString $ BSL.toChunks b) ()

-- |Same as 'unsafeBuilder' but marked as pre-escaped (e.g., for script tags and others marked external that suppress unescaped content).
preEscapedBuilder :: BSB.Builder -> B.Markup
preEscapedBuilder = preEscapedLazyByteString . BSB.toLazyByteString

-- |Render a pre-encoded (but not pre-escaped) 'BS.ByteString'.
-- The difference between this and 'B.unsafeByteString' is this does entity escaping.
byteString :: BS.ByteString -> B.Markup
byteString = unsafeBuilder . BW.fromHtmlEscapedByteString

-- |Render a pre-encoded (but not pre-escaped) lazy 'BSL.ByteString'.
-- The difference between this and 'B.unsafeLazyByteString' is this does entity escaping.
lazyByteString :: BSL.ByteString -> B.Markup
lazyByteString = unsafeBuilder . BW.fromHtmlEscapedLazyByteString

-- |Render a pre-encoded (but not pre-escaped) 'BSB.Builder'.
-- The difference between this and 'unsafeBuilder' is this does entity escaping.
builder :: BSB.Builder -> B.Markup
builder = lazyByteString . BSB.toLazyByteString

-- |Render text.
-- Identical to 'B.text' but about twice as fast.
text :: T.Text -> B.Markup
text = unsafeBuilder . BW.fromHtmlEscapedText

-- |Render lazy text.
-- Identical to 'B.lazyText' but about twice as fast.
lazyText :: TL.Text -> B.Markup
lazyText = unsafeBuilder . BW.fromHtmlEscapedLazyText

-- |Insert a 'BSB.Builder' as an attribute value. See 'B.unsafeByteString' for caveats.
unsafeBuilderValue :: BSB.Builder -> B.AttributeValue
unsafeBuilderValue = B.unsafeLazyByteStringValue . BSB.toLazyByteString

-- |Render a pre-encoded (but not pre-escaped) 'BS.ByteString' as an attribute value.
-- The difference between this and 'B.unsafeByteString' is this does entity escaping.
byteStringValue :: BS.ByteString -> B.AttributeValue
byteStringValue = unsafeBuilderValue . BW.fromHtmlEscapedByteString

-- |Render a pre-encoded (but not pre-escaped) lazy 'BSL.ByteString' as an attribute value.
-- The difference between this and 'B.unsafeLazyByteString' is this does entity escaping.
lazyByteStringValue :: BSL.ByteString -> B.AttributeValue
lazyByteStringValue = unsafeBuilderValue . BW.fromHtmlEscapedLazyByteString

-- |Render a pre-encoded (but not pre-escaped) 'BSB.Builder' as an attribute value.
-- The difference between this and 'unsafeBuilderValue' is this does entity escaping.
builderValue :: BSB.Builder -> B.AttributeValue
builderValue = lazyByteStringValue . BSB.toLazyByteString

-- |Render text as an attribute value.
-- Identical to 'B.textValue' but about twice as fast.
textValue :: T.Text -> B.AttributeValue
textValue = unsafeBuilderValue . BW.fromHtmlEscapedText

-- |Render lazy text as an attribute value.
-- Identical to 'B.lazyTextValue' but about twice as fast.
lazyTextValue :: TL.Text -> B.AttributeValue
lazyTextValue = unsafeBuilderValue . BW.fromHtmlEscapedLazyText

-- |Efficiently render a 'R.RouteAction' URI and query as an attribute value.
routeActionValue :: R.RouteAction r a -> r -> Query -> B.AttributeValue
routeActionValue r a q = builderValue -- R.routeURL Nothing r a $ toQuery q
  $  bh (R.requestHost rr)
  <> bp (R.requestPath rr)
  <> renderQueryBuilder True ((simpleQueryToQuery $ R.paramsQuerySimple $ R.requestQuery rr) ++ q)
  where
  rr = R.requestActionRoute r a
  bh [] = mempty
  bh [x] = BSB.byteString x
  bh (x:l) = bh l <> BSB.char8 '.' <> BSB.byteString x
  bp [] = BSB.char8 '/'
  bp p = encodePathSegments p

(!?) :: Markup.Attributable h => h -> Maybe B.Attribute -> h
h !? Nothing = h
h !? (Just a) = h B.! a
