module Main (main) where

import Spec (spec)
import Test.Hspec

main :: IO ()
main = hspec spec 