module Main (main) where

import           Control.Monad (unless)
import           GHC.IO.Encoding (utf8)
import           System.Exit (exitFailure)
import           System.IO (hSetEncoding, stdout, stderr)

import qualified IntMinMaxQueueSpec

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  passed <- IntMinMaxQueueSpec.tests
  unless passed exitFailure
