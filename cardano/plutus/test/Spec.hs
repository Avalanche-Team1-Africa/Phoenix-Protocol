module Main where

import qualified IntentRegistry.Tests as IntentRegistry
import qualified RecoveryModule.Tests as RecoveryModule
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Phoenix Protocol Tests"
  [ IntentRegistry.tests
  , RecoveryModule.tests
  ]