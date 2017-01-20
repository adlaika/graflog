{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Graflog.LoggerSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Except
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import Graflog.Logger
import Graflog.Console

mkFixture "Fixture" [''Console]

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
