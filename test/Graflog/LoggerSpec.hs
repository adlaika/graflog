{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Graflog.LoggerSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Except
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Data.Text
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec

import Graflog.Logger
import Graflog.Console

mkFixture "Fixture" [''Console]

data User = User
  { password :: Protected Text
  , email :: Text
  } deriving (Eq, Show)

deriveJSON defaultOptions ''User

data UserPair = UserPair
  { user1 :: User
  , user2 :: User
  } deriving (Eq, Show)

deriveJSON defaultOptions ''UserPair

spec :: Spec
spec = do
  describe "logEvent'" $ do
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
      result <- logTestFixtureT (logEvent' stubEvent) fixture
      result `shouldBe` ["writeStdout called" :: String]

  describe "Protected serialization" $ do
    let protectedUser = User
          { password = Protected ("rosebud" :: Text)
          , email = "test@test.com"
          }
    let expectedUser = object
          [ "password" .= ("(REDACTED)" :: Text)
          , "email" .= ("test@test.com" :: Text)
          ]
          
    it "should redact Protected text" $ do
      let superSecret = Protected ("the ruskies have the bomb" :: Text)
      let expected = "(REDACTED)"
      toJSON superSecret `shouldBe` expected

    it "should redact Protected record fields" $ do
      toJSON protectedUser `shouldBe` expectedUser

    it "should redact nested Protected records" $ do
      let usergroup = UserPair
            { user1 = protectedUser
            , user2 = protectedUser
            }
      let expected = object
            [ "user1" .= expectedUser
            , "user2" .= expectedUser
            ]
      toJSON usergroup `shouldBe` expected
