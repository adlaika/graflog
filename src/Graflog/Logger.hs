{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Graflog.Logger
  ( Logger(..)
  , Event(..)
  , CorrelationId(..)
  , EventId(..)
  , Action(..)
  , Protected(..)
  , logEvent'
  , jsonEncode
  ) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.Text (Text(..))
import Data.Text.Conversions (decodeConvertText, UTF8(..))

import Graflog.Console

newtype Protected a = Protected { unProtect :: a }
  deriving (Functor, Show, Eq)

instance ToJSON a => ToJSON (Protected a) where
  toJSON (Protected a) = "(REDACTED)"

instance FromJSON a => FromJSON (Protected a) where
  parseJSON v = Protected <$> parseJSON v

class Monad m => Logger m where
  logEvent :: Event -> m ()

newtype CorrelationId = CorrelationId Integer
  deriving (Eq, Show, Num, FromJSON, ToJSON)

newtype EventId = EventId Integer
  deriving (Eq, Show, Num, FromJSON, ToJSON)

newtype Action = Action Text
  deriving (Eq, Show, IsString, FromJSON, ToJSON)

data Event = Event
  { _correlationId :: CorrelationId
  , _eventId :: EventId
  , _action :: Action
  , _message :: Value
  } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Event

logEvent' :: Console m => Event -> m ()
logEvent' = writeStdout . jsonEncode

-- this will always work b/c UTF8 spec
jsonEncode :: ToJSON a => a -> Text
jsonEncode = byteStringToText . toStrict . encode
  where byteStringToText :: ByteString -> Text
        byteStringToText bs = fromJust $ decodeConvertText (UTF8 (bs :: ByteString))

instance Logger IO where
  logEvent = logEvent'
