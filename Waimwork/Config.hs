-- |Configuration parser and data.
--
-- The format of configuration files:
--
-- > FILE  ::= PAIRS
-- > PAIRS ::= PAIR*
-- > PAIR  ::= KEY ('=' VALUE | '{' PAIRS '}')
-- > KEY   ::= IDENT ('.' IDENT)*
-- > IDENT ::= [A-Za-z][A-Za-z0-9-_]*
-- > VALUE ::= ('true' | 'false' | [0-9]+ | '"' STR '"' | '[' (VALUE (',' VALUE)*)? ']')
-- > STR   ::= Haskell-syntax quoted string literal
--
-- For example, the following are equivalent:
--
-- > a.b = 1
-- > a { b = 1 }
--
-- Any \'#\' comments out the rest of the current line, but otherwise whitespace and newlines are ignored.
-- This configuration format is based on the configurator package and should be generally compatible.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Waimwork.Config
  (
  -- * Configuration data representation
    Key
  , Path(..)
  , pathKey
  , keyPath
  , Value(..)
  , ConfigError
  , ConfigMap(..)
  -- * Construction and access
  , Config
  , configMap
  , configPath
  , load
  , Configurable(..)
  , (!)
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Arrow (first, (***))
import Control.Exception (Exception, throw)
import Control.Monad ((<=<), guard)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable, TypeRep, typeRep)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Network.URI (URI, parseURI)
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString.Lazy as P
import qualified Text.Parsec.Token as PT

-- |The string representation of a configuration KEY.
type Key = BS.ByteString
-- |A (dot-separated) KEY path to a configuration value.
newtype Path = Path { pathList :: [Key] } deriving (Semigroup, Monoid)

-- |Convert a 'Path' to a dot-delimited KEY.
pathKey :: Path -> Key
pathKey (Path p) = BS.intercalate (BSC.singleton '.') p

-- |Split a dot-delimited KEY to a 'Path'.
keyPath :: Key -> Path
keyPath = Path . BSC.split '.'

pathSnoc :: Path -> Key -> Path
pathSnoc (Path l) k = Path (l ++ [k])

instance Show Path where
  showsPrec p = showsPrec p . pathKey

-- |Uses 'keyPath'
instance IsString Path where
  fromString = keyPath . fromString

-- |Errors that can be generated while parsing a configuration file
data ConfigError
  = ParseError P.ParseError
  | ConflictError
    { errorPath :: Path
    , errorValue1, errorValue2 :: Value
    }
  | ValueError
    { errorPath :: Path
    , errorValue :: Value
    , errorNeeded :: TypeRep
    }
  deriving (Typeable, Show)

instance Exception ConfigError

-- |Configuration VALUEs.
data Value
  = Empty -- ^Any key not explicitly given a value is treated as 'Empty'.
  | Boolean !Bool
  | Integer !Integer
  | String !BS.ByteString
  | List [Value]
  | Sub !ConfigMap
  deriving (Typeable, Eq, Show)

-- |A single layer of configuration
newtype ConfigMap = ConfigMap { unConfigMap :: HM.HashMap Key Value }
  deriving (Typeable, Eq, Semigroup, Monoid, Show)

-- |A loaded configuration or sub-configuration
data Config = Config
  { configPath :: !Path -- ^The path to this sub-configuration (empty for top level)
  , configMap :: !ConfigMap -- ^The configured values at this level
  } deriving (Typeable, Show)

topConfig :: ConfigMap -> Config
topConfig = Config (Path [])

unionValue :: Path -> Value -> Value -> Value
unionValue _ Empty v = v
unionValue _ v Empty = v
unionValue p (Sub m1) (Sub m2) = Sub $ unionConfig p m1 m2
unionValue p v1 v2
  | v1 == v2 = v1
  | otherwise = throw $ ConflictError{ errorPath = p, errorValue1 = v1, errorValue2 = v2 }

unionConfig :: Path -> ConfigMap -> ConfigMap -> ConfigMap
unionConfig p (ConfigMap m1) (ConfigMap m2) = ConfigMap $ HM.foldrWithKey (\k -> HM.insertWith (flip $ unionValue (pathSnoc p k)) k) m1 m2

instance Semigroup Config where
  Config (Path p1) m1 <> Config (Path p2) m2 = Config p m where
    (p', (p1', p2')) = cpfx p1 p2
    p = Path p'
    m = unionConfig p (nest m1 p1') (nest m2 p2')
    cpfx (a:al) (b:bl) | a == b = first (a :) $ cpfx al bl
    cpfx al bl = ([], (al, bl))
    nest = foldr (\k -> ConfigMap . HM.singleton k . Sub)

-- |Merge two configs, throwing 'ConflictError' on conflicts
instance Monoid Config where
  mempty = topConfig mempty
  mappend = (<>)

lookup :: Path -> ConfigMap -> Value
lookup (Path []) m = Sub m
lookup (Path [k]) (ConfigMap m) | Just v <- HM.lookup k m = v
lookup (Path (k:l)) (ConfigMap m) | Just (Sub km) <- HM.lookup k m = lookup (Path l) km
lookup _ _ = Empty

parser :: P.Parser ConfigMap
parser = whiteSpace *> block mempty mempty <* P.eof where
  block p m = (block p =<< pair p m) <|> return (ConfigMap m)
  pair p m = do
    ks <- identifier P.<?> "key"
    let k = BSC.pack ks
        kp = pathSnoc p k
    km <- case HM.lookupDefault Empty k m of
      Empty -> return Nothing
      Sub (ConfigMap km) -> return $ Just km
      _ -> fail $ "Duplicate key value: " ++ show kp
    kv <- lexeme dot *> (Sub . ConfigMap <$> pair kp (fold km)) <|> rhs kp km
    return $ HM.insert k kv m
  rhs p Nothing = sub p HM.empty <|>
    lexeme (P.char '=') *> val
  rhs p (Just m) = sub p m
  sub p m = Sub <$> braces (block p m)
  val = P.choice
    [ Boolean True <$ reserved "true"
    , Boolean False <$ reserved "false"
    , Integer <$> integer
    , String . BSC.pack <$> stringLiteral
    , List <$> brackets (commaSep val)
    ] P.<?> "value"
  PT.TokenParser{..} = PT.makeTokenParser PT.LanguageDef
    { PT.commentStart = ""
    , PT.commentEnd = ""
    , PT.commentLine = "#"
    , PT.nestedComments = False
    , PT.identStart = P.letter
    , PT.identLetter = (P.alphaNum <|> P.oneOf "-_")
    , PT.opStart = P.unexpected "operator"
    , PT.opLetter = P.unexpected "operator"
    , PT.reservedNames = []
    , PT.reservedOpNames = ["="]
    , PT.caseSensitive = True
    }

-- |Load a configuration file (or throw a 'ConfigError').
load :: FilePath -> IO Config
load f = either (throw . ParseError) (return . topConfig) =<< P.parseFromFile parser f

-- |A value that can be retrieved from a configuration
class Typeable a => Configurable a where
  -- |Retrieve and convert a value from a 'Config' by 'Path'
  get :: Path -> Config -> a
  get p (Config cp m) = fromMaybe (throw ValueError{ errorPath = cp <> p, errorValue = v, errorNeeded = typeRep r}) r where
    v = lookup p m
    r = config v
  -- |Convert a configuration value
  config :: Value -> Maybe a

instance Configurable Value where
  get p (Config _ m) = lookup p m
  config = Just

instance Configurable ConfigMap where
  config (Sub m) = Just m
  config Empty = Just mempty
  config _ = Nothing

instance Configurable Config where
  get p c = Config (configPath c <> p) $ get p c
  config v = topConfig <$> config v

instance Configurable a => Configurable (Maybe a) where
  config Empty = Just Nothing
  config v = Just <$> config v

-- |Left-baised choice between types.
instance (Configurable a, Configurable b) => Configurable (Either a b) where
  config v = Left <$> config v <|> Right <$> config v

instance Configurable Bool where
  config (Boolean b) = Just b
  config _ = Nothing

instance Configurable Integer where
  config (Integer i) = Just i
  config _ = Nothing

instance Configurable BS.ByteString where
  config (String s) = Just s
  config _ = Nothing

instance {-# OVERLAPPABLE #-} Configurable a => Configurable [a] where
  config (List l) = mapM config l
  config _ = Nothing

instance Configurable T.Text where
  config = either (const Nothing) Just . TE.decodeUtf8' <=< config

instance {-# OVERLAPPING #-} Configurable String where
  config v = BSC.unpack <$> config v

configBoundedInt :: forall a . (Integral a, Bounded a) => Value -> Maybe a
configBoundedInt = f <=< config where
  f i = fromInteger i
    <$ guard (i >= toInteger (minBound :: a) && i <= toInteger (maxBound :: a))

instance Configurable Int    where config = configBoundedInt
instance Configurable Int8   where config = configBoundedInt
instance Configurable Int16  where config = configBoundedInt
instance Configurable Int32  where config = configBoundedInt
instance Configurable Int64  where config = configBoundedInt
instance Configurable Word   where config = configBoundedInt
instance Configurable Word8  where config = configBoundedInt
instance Configurable Word16 where config = configBoundedInt
instance Configurable Word32 where config = configBoundedInt
instance Configurable Word64 where config = configBoundedInt

instance Configurable URI where
  config = parseURI <=< config

infixl 9 !
-- |Retrieve a value from a 'Config' by 'Path' (@'flip' 'get'@)
(!) :: Configurable a => Config -> Path -> a
(!) = flip get

instance JSON.ToJSON Config where
  toJSON = JSON.toJSON . configMap
  toEncoding = JSON.toEncoding . configMap

instance JSON.ToJSON ConfigMap where
  toJSON = JSON.object . map (TE.decodeUtf8 *** JSON.toJSON) . HM.toList . unConfigMap
  toEncoding = JSON.pairs . HM.foldrWithKey (\k v -> (TE.decodeUtf8 k JSON..= v <>)) mempty . unConfigMap

instance JSON.ToJSON Value where
  toJSON Empty = JSON.Null
  toJSON (Boolean b) = JSON.Bool b
  toJSON (String s) = JSON.String $ TE.decodeUtf8 s
  toJSON (Integer i) = JSON.Number $ fromInteger i
  toJSON (List l) = JSON.Array $ V.fromList $ map JSON.toJSON l
  toJSON (Sub c) = JSON.toJSON c
  toEncoding (List l) = JSON.foldable l
  toEncoding (Sub c) = JSON.toEncoding c
  toEncoding v = JSON.toEncoding $ JSON.toJSON v

instance JSON.FromJSON Config where
  parseJSON j = topConfig <$> JSON.parseJSON j

instance JSON.FromJSON ConfigMap where
  parseJSON j = ConfigMap . HM.foldlWithKey' (\m k v -> HM.insert (TE.encodeUtf8 k) v m) HM.empty <$> JSON.parseJSON j

instance JSON.FromJSON Value where
  parseJSON JSON.Null = return Empty
  parseJSON (JSON.Bool b) = return $ Boolean b
  parseJSON (JSON.String s) = return $ String $ TE.encodeUtf8 s
  parseJSON j@(JSON.Number _) = Integer <$> JSON.parseJSON j
  parseJSON j@(JSON.Array _) = List <$> JSON.parseJSONList j
  parseJSON j@(JSON.Object _) = Sub <$> JSON.parseJSON j
