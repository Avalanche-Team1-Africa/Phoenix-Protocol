{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module IntentRegistry.Tests where

import           Control.Monad              (void)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Ledger                     hiding (singleton)
import           Ledger.Ada                 as Ada
import           Ledger.TimeSlot            (slotToBeginPOSIXTime)
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Test.Tasty
import           Test.Tasty.HUnit

import           IntentRegistry

tests :: TestTree
tests = testGroup "Intent Registry Tests"
  [ testCase "Register intent" testRegisterIntent
  , testCase "Execute intent" testExecuteIntent
  , testCase "Cancel intent" testCancelIntent
  , testCase "Execute expired intent fails" testExecuteExpiredIntent
  , testCase "Execute already executed intent fails" testExecuteAlreadyExecutedIntent
  ]

-- Test data
testOwner :: PubKeyHash
testOwner = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

testAction :: BuiltinByteString
testAction = "swap"

testParameters :: BuiltinByteString
testParameters = "token1,token2,100"

testTimestamp :: POSIXTime
testTimestamp = 1_000_000

testExpiry :: POSIXTime
testExpiry = 2_000_000

testIntentId :: BuiltinByteString
testIntentId = "intent-123"

testIntent :: Intent
testIntent = Intent
  { intentOwner = testOwner
  , intentAction = testAction
  , intentParameters = testParameters
  , intentTimestamp = testTimestamp
  , intentExpiry = testExpiry
  }

testIntentDatum :: IntentDatum
testIntentDatum = IntentDatum
  { intentId = testIntentId
  , intent = testIntent
  , isExecuted = False
  }

-- Test registering an intent
testRegisterIntent :: IO ()
testRegisterIntent = do
  let validator = mkIntentValidator
      redeemer = RegisterIntent testIntent
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Register intent should succeed" (validator testIntentDatum redeemer context)

-- Test executing an intent
testExecuteIntent :: IO ()
testExecuteIntent = do
  let validator = mkIntentValidator
      redeemer = ExecuteIntent testIntentId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Execute intent should succeed" (validator testIntentDatum redeemer context)

-- Test cancelling an intent
testCancelIntent :: IO ()
testCancelIntent = do
  let validator = mkIntentValidator
      redeemer = CancelIntent testIntentId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Cancel intent should succeed" (validator testIntentDatum redeemer context)

-- Test executing an expired intent
testExecuteExpiredIntent :: IO ()
testExecuteExpiredIntent = do
  let validator = mkIntentValidator
      redeemer = ExecuteIntent testIntentId
      context = scriptContextMock testOwner 3_000_000 -- After expiry
  
  assertBool "Execute expired intent should fail" (not $ validator testIntentDatum redeemer context)

-- Test executing an already executed intent
testExecuteAlreadyExecutedIntent :: IO ()
testExecuteAlreadyExecutedIntent = do
  let executedDatum = testIntentDatum { isExecuted = True }
      validator = mkIntentValidator
      redeemer = ExecuteIntent testIntentId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Execute already executed intent should fail" (not $ validator executedDatum redeemer context)

-- Helper function to create a mock script context
scriptContextMock :: PubKeyHash -> POSIXTime -> ScriptContext
scriptContextMock signer currentTime =
  ScriptContext
    { scriptContextTxInfo = TxInfo
        { txInfoInputs = []
        , txInfoOutputs = []
        , txInfoFee = Ada.lovelaceValueOf 10_000_000
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = Map.empty
        , txInfoValidRange = POSIXTimeRange (LowerBound (Finite currentTime) True) (UpperBound PosInf True)
        , txInfoSignatories = [signer]
        , txInfoData = []
        , txInfoId = "mock-tx-id"
        , txInfoReferenceInputs = []
        }
    , scriptContextPurpose = Spending "mock-output-ref"
    }