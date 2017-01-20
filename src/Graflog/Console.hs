module Graflog.Console
  ( Console(..)
  , writeStdout'
  , enableStdoutLineBuffering
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import System.IO (hSetBuffering, stdout, BufferMode(..))

class Monad m => Console m where
  writeStdout :: Text -> m ()

writeStdout' :: MonadIO m => Text -> m ()
writeStdout' = liftIO . T.putStrLn

-- this function forces stdout into line buffering mode; useful for CloudFormation logs
enableStdoutLineBuffering :: MonadIO m => m ()
enableStdoutLineBuffering = liftIO $ hSetBuffering stdout LineBuffering

instance Console IO where
  writeStdout = writeStdout'
