{-# LANGUAGE OverloadedStrings #-}

module TestRunner (main) where

import           Test.Hspec
import           Spec (spec)
import qualified IntegrationTest

main :: IO ()
main = hspec $ do
  -- Run unit tests
  spec
  
  -- Run integration tests
  IntegrationTest.spec 