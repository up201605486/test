
module Main where

import Tests (runTests)
import System.Exit

main :: IO ()
main = do
  passed <- runTests
  if passed then exitSuccess else exitFailure
  
