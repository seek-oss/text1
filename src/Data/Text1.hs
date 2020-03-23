{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Text1
  ( Text1
  , text1ToText
  , textToText1
  , textToText1M
  , cons
  , unsafeCons
  , head
  , last
  , intercalate
  , replace
  , splitOn
  , concat
  , length
  , unpack
  , toLower
  , toUpper
  , toTitle
  , toCaseFold
  , words
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , filter
  , map
  , ToText1(..)
  , toText
  , toString
  , FromText1(..)
  , fromText1
  , fromText1M
  , fromText
  , fromTextM
  , fromString
  , fromStringM
  , takeText1
  , takeCIText1
  , CI(..)
  , fromText1Error
  , unsafeFromText
  , Text
  , asText1
  , _Text1
  , AsText1(..)
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (Iso', Prism', iso, prism')
import           Control.Monad ((<=<))
import           Control.Monad.Fail (MonadFail(..))
import           Data.Aeson
                  (FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..), ToJSON(..),
                  ToJSONKey(..), Value(..), withText)
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor (first)
import           Data.CaseInsensitive (CI(..), FoldCase(..))
import qualified Data.CaseInsensitive as CI
import           Data.Coerce (coerce)
import           Data.Csv (FromField(..), ToField(..))
import           Data.Data (Data)
import           Data.Fixed (Fixed, HasResolution)
import           Data.Hashable (Hashable(..))
import           Data.Int (Int64)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Scientific (Scientific)
import           Data.Semigroup (sconcat)
import           Data.String (IsString)
import qualified Data.String as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Typeable (Typeable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           GHC.Generics (Generic)
import           GHC.Read (Read(..), lexP, parens)
import           GHC.Stack (HasCallStack)
import           Network.AWS.Data.Numeric (Nat)
import           Network.AWS.Data.Text (FromText(..))
import qualified Network.AWS.Data.Text as AWS
import           Numeric.Natural (Natural)
import           Prelude hiding (concat, fail, filter, head, last, length, map, words)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()
import           Text.Read.Lex (Lexeme(Ident))

newtype Text1 = UnsafeText1 Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Hashable Text1 where
  hashWithSalt salt = hashWithSalt salt . text1ToText

instance NFData Text1 where
  rnf !_ = ()

instance Show Text1 where
  show = show . text1ToText

instance Read Text1 where
  readPrec = parens $ do
    Ident s <- lexP
    textToText1M $ T.pack s

instance IsString Text1 where
  fromString = \case
    ""  -> error "IsString: Text1 from empty string literal."
    str -> coerce T.pack str
  {-# INLINE fromString #-}

instance Semigroup Text1 where
  (<>) = coerce ((<>) @Text)
  {-# INLINE (<>) #-}

instance Arbitrary Text1 where
  arbitrary =
    unsafeCons
      <$> arbitrary
      <*> arbitrary
  shrink = mapMaybe textToText1M . shrink . text1ToText

instance ToField Text1 where
  toField = encodeUtf8 . text1ToText

instance FromField Text1 where
  parseField = textToText1M <=< either (fail . show) pure . decodeUtf8'

instance {-# OVERLAPPING #-} FromField (Maybe Text1) where
  parseField = fmap textToText1M . parseField
  {-# INLINE parseField #-}

instance ToJSONKey Text1 where
  toJSONKey = toJSONKeyText text1ToText

instance FromJSONKey Text1 where
  fromJSONKey = FromJSONKeyTextParser fromTextM

instance ToJSON Text1 where
  toJSON = String . text1ToText

instance FromJSON Text1 where
  parseJSON = withText "Text1" textToText1M

instance ToHttpApiData Text1 where
  toUrlPiece = text1ToText

instance FromHttpApiData Text1 where
  parseUrlPiece = first T.pack . fromText

instance FoldCase Text1 where
  foldCase = toCaseFold

text1ToText :: Text1 -> Text
text1ToText = coerce
{-# INLINE text1ToText #-}

textToText1 :: Text -> Either String Text1
textToText1 text
  | T.null text = Left "Failure parsing Text1 from empty text."
  | otherwise = Right $ UnsafeText1 text

textToText1M :: MonadFail m => Text -> m Text1
textToText1M = either fail pure . textToText1

cons :: Char -> Text1 -> Text1
cons = coerce T.cons
{-# INLINE cons #-}

unsafeCons :: Char -> Text -> Text1
unsafeCons = coerce T.cons
{-# INLINE unsafeCons #-}

head :: Text1 -> Char
head = coerce T.head

last :: Text1 -> Char
last = coerce T.last

intercalate :: Text1 -> NonEmpty Text1 -> Text1
intercalate t = concat . NE.intersperse t

replace ::
  Text1 -- ^ @needle@ to search for.
  -> Text1 -- ^ @replacement@ to replace @needle@ with.
  -> Text1 -- ^ @haystack@ in which to search.
  -> Text1
replace needle replacement haystack =
  let needle' = text1ToText needle
      replacement' = text1ToText replacement
  in coerce (T.replace needle' replacement') haystack

splitOn :: Text1 -> Text1 -> [Text1]
splitOn pat = mapMaybe textToText1M . T.splitOn (text1ToText pat) . text1ToText

concat :: NonEmpty Text1 -> Text1
concat = sconcat

length :: Text1 -> Int
length = coerce T.length

unpack :: Text1 -> String
unpack = coerce T.unpack

toLower :: Text1 -> Text1
toLower = coerce T.toLower

toUpper :: Text1 -> Text1
toUpper = coerce T.toUpper

toTitle :: Text1 -> Text1
toTitle = coerce T.toTitle

toCaseFold :: Text1 -> Text1
toCaseFold = coerce T.toCaseFold

words :: Text1 -> [Text1]
words = mapMaybe textToText1M . T.words . text1ToText

isPrefixOf :: Text1 -> Text1 -> Bool
isPrefixOf = coerce T.isPrefixOf

isSuffixOf :: Text1 -> Text1 -> Bool
isSuffixOf = coerce T.isSuffixOf

isInfixOf :: Text1 -> Text1 -> Bool
isInfixOf = coerce T.isInfixOf

filter :: (Char -> Bool) -> Text1 -> Maybe Text1
filter predicate = textToText1M . coerce (T.filter predicate)

map :: (Char -> Char) -> Text1 -> Maybe Text1
map f = textToText1M . T.map f . text1ToText

fromText1 :: FromText1 a => Text1 -> Either String a
fromText1 = A.parseOnly parser1 . text1ToText

fromText1M :: (MonadFail m, FromText1 a) => Text1 -> m a
fromText1M = either fail pure . fromText1

fromText :: FromText1 a => Text -> Either String a
fromText = fromText1 <=< textToText1

fromTextM :: (MonadFail m, FromText1 a) => Text -> m a
fromTextM = either fail pure . fromText

fromString :: FromText1 a => String -> Either String a
fromString = fromText . T.pack

fromStringM :: (MonadFail m, FromText1 a) => String -> m a
fromStringM = fromTextM . T.pack

toText :: ToText1 a => a -> Text
toText = text1ToText . toText1

toString :: ToText1 a => a -> String
toString = T.unpack . toText

takeText1 :: Parser Text1
takeText1 = textToText1M =<< A.takeText

takeCIText1 :: Parser (CI Text1)
takeCIText1 = CI.mk <$> takeText1

-- | Fail parsing with a 'Text1' error.
--
-- Constrained to the actual attoparsec monad to avoid
-- exposing 'fail' usage directly.
fromText1Error :: Text1 -> Parser a
fromText1Error = fail . unpack

unsafeFromText :: HasCallStack => Text -> Text1
unsafeFromText = either error id . textToText1

asText1 :: Iso' Text (Maybe Text1)
asText1 = iso textToText1M (maybe "" text1ToText)

_Text1 :: Prism' Text Text1
_Text1 = prism' text1ToText textToText1M

class FromText1 a where
  parser1 :: Parser a

instance FromText1 Double where
  parser1 = parser

instance FromText1 Int where
  parser1 = parser

instance FromText1 Int64 where
  parser1 = parser

instance FromText1 Integer where
  parser1 = parser

instance FromText1 Natural where
  parser1 = parser

instance FromText1 Nat where
  parser1 = parser

instance FromText1 Scientific where
  parser1 = parser

instance HasResolution a => FromText1 (Fixed a) where
  parser1 = A.signed A.rational <* A.endOfInput

instance FromText1 Text1 where
  parser1 = takeText1

instance FromText1 Text where
  parser1 = parser

instance FromText1 String where
  parser1 = parser

instance FromText1 Bool where
  parser1 = parser

instance FromText1 UUID where
  parser1 = tryParse . text1ToText =<< takeText1
    where
      tryParse text =
        maybe (fail $ "Cannot parse UUID from '" <> T.unpack text <> "'.") pure $
          UUID.fromText text

class ToText1 a where
  toText1 :: a -> Text1

instance ToText1 Double where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Int where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Int64 where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Integer where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Natural where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Nat where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Scientific where
  toText1 = unsafeFromText . AWS.toText

instance HasResolution a => ToText1 (Fixed a) where
  toText1 = unsafeFromText . T.pack . show

instance ToText1 Text1 where
  toText1 = id

instance ToText1 String where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 Bool where
  toText1 = unsafeFromText . AWS.toText

instance ToText1 UUID where
  toText1 = unsafeFromText . UUID.toText

-- | 'AsText' allows you to derive instances for your own types utilising their
-- 'ToText1' and 'FromText1' instances using 'deriving via'.
-- You could derive 'FromJSON' via 'AsText1' and this would use the 'FromText1'
-- instance, giving you greater control over your type instances without all the boiler plate
-- for each instance.
--
-- > data Jedi
-- >   = JediObiWanKenobi
-- >   | JediLuke
-- >   | JediYoda
-- >   deriving (Eq, Ord, Generic, Bounded, Enum)
-- >   deriving (Show, Read, ToJSON, FromJSON) via AsText1 Jedi
-- >
-- > instance FromText1 Jedi where
-- >   parser1 = takeCIText1 >>= \case
-- >     "ObiWanKenobi" -> pure JediObiWanKenobi
-- >     "Luke"         -> pure JediLuke
-- >     "Yoda"         -> pure JediYoda
-- >     e              -> fromText1Error $ "The force is not strong with you: " <> original e
-- >
-- > instance ToText1 Jedi where
-- >   toText1 = \case
-- >     JediObiWanKenobi -> "ObiWanKenobi"
-- >     JediLuke         -> "Luke"
-- >     JediYoda         -> "Yoda"
newtype AsText1 a = AsText1
  { _asText1 :: a }
  deriving (Eq)
  deriving newtype (ToText1, FromText1) -- 🤯

instance ToText1 a => Show (AsText1 a) where
  show = toString

instance FromText1 a => Read (AsText1 a) where
  readPrec = fromText1M =<< readPrec

instance ToText1 a => Hashable (AsText1 a) where
  hashWithSalt salt = hashWithSalt salt . toText1

instance FromText1 a => IsString (AsText1 a) where
  fromString ""  = error "IsString: Text1 from empty string literal."
  fromString str = either error id $ fromString str
  {-# INLINE fromString #-}

instance ToText1 a => ToField (AsText1 a) where
  toField = toField . toText1

instance FromText1 a => FromField (AsText1 a) where
  parseField = fromText1M <=< parseField

instance ToText1 a => ToJSONKey (AsText1 a) where
  toJSONKey = toJSONKeyText toText

instance FromText1 a => FromJSONKey (AsText1 a) where
  fromJSONKey = FromJSONKeyTextParser fromTextM

instance ToText1 a => ToJSON (AsText1 a) where
  toJSON = toJSON . toText1

instance FromText1 a => FromJSON (AsText1 a) where
  parseJSON = fromText1M <=< parseJSON

instance ToText1 a => ToHttpApiData (AsText1 a) where
  toUrlPiece = toUrlPiece . toText1

instance FromText1 a => FromHttpApiData (AsText1 a) where
  parseUrlPiece = first T.pack . fromText1 <=< parseUrlPiece
