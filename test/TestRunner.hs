{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec.Runner
import           Spec (spec)
import qualified IntegrationTest

main :: IO ()
main = hspecWith defaultConfig { configPrintCpuTime = True } $ do
  -- Run unit tests
  spec

  -- Run integration tests
  IntegrationTest.spec 