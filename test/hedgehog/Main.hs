module Main (main) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO (hSetEncoding, stdout, stderr)

import qualified IntMinMaxQueueSpec
import qualified MinMaxQueueSpec

main :: IO ()
main = do
  passed <- sequenceA [MinMaxQueueSpec.tests, IntMinMaxQueueSpec.tests]
  unless (and passed) exitFailure
