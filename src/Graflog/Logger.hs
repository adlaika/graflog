{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graflog.Logger
  ( Logger(..)
  , Event(..)
  , CorrelationId(..)
  , EventId(..)
  , Action(..)
  , logJSON'
  , jsonEncode
  , ToLog(..)
  , Log(..)
  , numToLog
  , dictionary
  , pair
  ) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust)
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.String (IsString(..))
import Data.Text (Text(..))
import Data.Text.Conversions (decodeConvertText, UTF8(..), toText)

import Graflog.Console

data Log
  = Dictionary (Map Text Log)
  | Variant Text [Log]
  | List [Log]
  | Message Text
  | Redacted
  deriving (Eq, Show)

instance IsString Log where
  fromString = Message . toText

class ToLog a where
  toLog :: a -> Log

instance ToLog Log where
  toLog = id

instance ToLog Text where
  toLog = Message

instance ToLog a => ToLog [a] where
  toLog = List . fmap toLog

numToLog :: (Show a, Num a) => a -> Log
numToLog = Message . toText . show

instance ToLog Int where
  toLog = numToLog

instance ToLog Integer where
  toLog = numToLog

instance ToLog Float where
  toLog = numToLog

instance ToLog Double where
  toLog = numToLog

instance ToLog a => ToLog (Map Text a) where
  toLog = Dictionary . fmap toLog

instance (ToLog a, ToLog b) => ToLog (Either a b) where
  toLog (Left l) = Variant "left" [toLog l]
  toLog (Right r) = Variant "right" [toLog r]

instance ToLog a => ToLog (Maybe a) where
  toLog (Just a) = Variant "some" [toLog a]
  toLog Nothing = Variant "none" []

dictionary :: [(Text, Log)] -> Log
dictionary = Dictionary . Map.fromList

pair :: ToLog a => Text -> a -> (Text, Log)
pair key value = (key, toLog value)

redacted :: Text
redacted = "(REDACTED)"

instance ToJSON Log where
  toJSON (Message a) = String a
  toJSON (List a) = toJSON a
  toJSON (Dictionary a) = toJSON a
  toJSON (Variant tag values) = toJSON $ Map.fromList [(tag, map toJSON values)]
  toJSON Redacted = String redacted

class Monad m => Logger m where
  logJSON :: Event -> m ()

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
  , _message :: Log
  } deriving (Eq, Show)

instance ToJSON Event where
  toJSON Event{_correlationId, _eventId, _action, _message} =
    object
      [ "correlationId" .= _correlationId
      , "eventId" .= _eventId
      , "action" .= _action
      , "message" .= _message
      ]

logJSON' :: Console m => Event -> m ()
logJSON' = writeStdout . jsonEncode

-- this will always work b/c UTF8 spec
jsonEncode :: ToJSON a => a -> Text
jsonEncode = byteStringToText . toStrict . encode
  where byteStringToText :: ByteString -> Text
        byteStringToText bs = fromJust $ decodeConvertText (UTF8 (bs :: ByteString))

instance Logger IO where
  logJSON = logJSON'
