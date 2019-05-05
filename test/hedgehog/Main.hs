module Main (main) where

import           Control.Monad (unless)
import           GHC.IO.Encoding (utf8)
import           System.Exit (exitFailure)
import           System.IO (hSetEncoding, stdout, stderr)

import qualified IntMinMaxQueueSpec
import qualified MinMaxQueueSpec

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  passed <- sequenceA [MinMaxQueueSpec.tests, IntMinMaxQueueSpec.tests]
  unless (and passed) exitFailure
