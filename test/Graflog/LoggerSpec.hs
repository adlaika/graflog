{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Graflog.LoggerSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Except
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Data.Text
import Data.Aeson
import Data.Map (Map(..))
import qualified Data.Map as Map
import Test.Hspec

import Graflog.Logger
import Graflog.Console

mkFixture "Fixture" [ts| Console |]

data User = User
  { password :: Text
  , email :: Text
  } deriving (Eq, Show)

instance ToLog User where
  toLog User{email, password} = dictionary
    [ pair "email" email
    , pair "password" Redacted
    ]

spec :: Spec
spec = do
  describe "logJSON'" $ do
    it "should call stdout with serialized json" $ do
      let stubEvent = Event
            { _correlationId = 1
            , _eventId = 2
            , _action = Action "action"
            , _message = "message"
            }
      let fixture = def
            { _writeStdout = \json -> do
                lift $ json `shouldBe` jsonEncode stubEvent
                log "writeStdout called"
            }
      result <- logTestFixtureT (logJSON' stubEvent) fixture
      result `shouldBe` ["writeStdout called" :: String]
  describe "toLog" $ do
    it "should convert Log to Log" $ do
      let loog = Message "hello"
      let expected = "hello"
      toLog loog `shouldBe` expected
    it "should convert () to empty Dictionary" $
      toLog () `shouldBe` Dictionary mempty
    it "should convert Text to Message" $ do
      let text = ("hello" :: Text)
      let expected = "hello"
      toLog text `shouldBe` expected
      True `shouldBe` True
    it "should convert list to List" $ do
      let list = [("boop" :: Text), "bop"]
      let expected = List ["boop", "bop"]
      toLog list `shouldBe` expected
    it "should convert num to Message" $ do
      let integer = (1234 :: Integer)
      let expected = "1234"
      toLog integer `shouldBe` expected
      let int = (123 :: Int)
      let expected = "123"
      toLog int `shouldBe` expected
      let float = (123.456 :: Float)
      let expected = "123.456"
      toLog float `shouldBe` expected
      let double = (234.567 :: Double)
      let expected = "234.567"
      toLog double `shouldBe` expected
    it "should convert map to Dictionary" $ do
      let mapp = (Map.fromList
            [ ("1", "one")
            , ("2", "two")
            ]) :: Map Text Text
      let expected = dictionary
            [ pair "1" (Message "one")
            , pair "2" (Message "two")
            ]
      toLog mapp `shouldBe` expected
    it "should convert record to Dictionary while redacting fields marked Redacted" $ do
      let user = User
            { email = "test@test.com"
            , password = "secret"
            }
      let expected = Dictionary (Map.fromList
            [ ("email", "test@test.com")
            , ("password", Redacted)
            ])
      toLog user `shouldBe` expected
    it "should convert Either to Variant" $ do
      let left = Left "error" :: Either Text Text
      let expected = Variant "left"  [Message "error"]
      toLog left `shouldBe` expected
      let right = Right "success" :: Either Text Text
      let expected = Variant "right" [Message "success"]
      toLog right `shouldBe` expected
    it "should convert Maybe to Variant" $ do
      let just = Just "data" :: Maybe Text
      let expected = Variant "some" [Message "data"]
      toLog just `shouldBe` expected
      let nothing = Nothing :: Maybe Text
      let expected = Variant "none" []
      toLog nothing `shouldBe` expected
  describe "ToJSON Log" $ do
    it "should convert Message to String" $ do
      let message = Message "hello"
      let expected = String "hello"
      toJSON message `shouldBe` expected
    it "should redact Redacted" $ do
      let redacted = Redacted
      let expected = String "(REDACTED)"
      toJSON redacted `shouldBe` expected
    it "should convert Variant to Object" $ do
      let variant = Variant "right" [Message "test"]
      let expected = object
            [ "right" .= ["test" :: Value]
            ]
      toJSON variant `shouldBe` expected
    it "should convert Dictionary to Object" $ do
      let dict = Dictionary (Map.fromList
            [ ("email", Message "test@test.com")
            , ("password", Redacted)
            ])
      let expected = object
            [ ("email" :: Text) .= ("test@test.com" :: Text)
            , ("password" :: Text) .= ("(REDACTED)" :: Text)
            ]
      toJSON dict `shouldBe` expected
    it "should convert List to Array" $ do
      let list = List [Message "foo", Message "bar"]
      let expected = toJSON (["foo", "bar"] :: [Text])
      toJSON list `shouldBe` expected
