import qualified Criterion.Main as C
import qualified Data.ByteString.Builder as B
import Data.List (mapAccumL)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Tuple (swap)
import System.Random (randoms, mkStdGen)
import qualified Text.Blaze.Renderer.Utf8 as BM

import qualified Blaze.ByteString.Builder.Html.Utf8 as BU
import qualified Blaze.ByteString.Builder.Html.Word as BW
import qualified Dwebework.Blaze as DB
import qualified Text.Blaze as TB

input :: [String]
input =
  ['\0'..'\255']
  : chr 128 ++ chr 256 ++ chr (succ $ fromEnum (maxBound :: Char))
  where
  lists = snd $ mapAccumL (\r l -> swap $ splitAt l r) (randoms $ mkStdGen 0) $ [0..80] ++ [100,500,1000,5000,10000]
  chr n = map (map (toEnum . (`mod` n))) lists

main :: IO ()
main = C.defaultMain
  [ C.env (return input) $ \s ->
    C.bgroup "String"
    [ C.env (return $ map T.pack s) $ \t ->
      C.bgroup "fromHtmlEscapedText"
      [ C.bench "Word" $ C.nf (map (B.toLazyByteString . BW.fromHtmlEscapedText)) t
      , C.bench "Utf8" $ C.nf (map (B.toLazyByteString . BU.fromHtmlEscapedText)) t
      ]
    , C.env (return $ map TL.pack s) $ \t ->
      C.bgroup "fromHtmlEscapedLazyText"
      [ C.bench "Word" $ C.nf (map (B.toLazyByteString . BW.fromHtmlEscapedLazyText)) t
      , C.bench "Utf8" $ C.nf (map (B.toLazyByteString . BU.fromHtmlEscapedLazyText)) t
      ]
    , C.env (return $ map T.pack s) $ \t ->
      C.bgroup "text"
      [ C.bench "Dwebework" $ C.nf (map (BM.renderMarkup . DB.text)) t
      , C.bench "Text" $ C.nf (map (BM.renderMarkup . TB.text)) t
      ]
    , C.env (return $ map TL.pack s) $ \t ->
      C.bgroup "lazyText"
      [ C.bench "Dwebework" $ C.nf (map (BM.renderMarkup . DB.lazyText)) t
      , C.bench "Text" $ C.nf (map (BM.renderMarkup . TB.lazyText)) t
      ]
    ]
  ]
