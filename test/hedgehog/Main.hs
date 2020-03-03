module Main (main) where

import           Control.Monad (unless)
import           GHC.IO.Encoding (utf16le)
import           System.Exit (exitFailure)
import           System.IO (hSetEncoding, stdout, stderr)

import qualified IntMinMaxQueueSpec
import qualified MinMaxQueueSpec

main :: IO ()
main = do
  hSetEncoding stdout utf16le
  hSetEncoding stderr utf16le
  passed <- sequenceA [MinMaxQueueSpec.tests, IntMinMaxQueueSpec.tests]
  unless (and passed) exitFailure
