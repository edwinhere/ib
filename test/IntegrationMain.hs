module IntegrationMain where

import Test.Hspec
import qualified IntegrationTest

main :: IO ()
main = hspec IntegrationTest.spec 