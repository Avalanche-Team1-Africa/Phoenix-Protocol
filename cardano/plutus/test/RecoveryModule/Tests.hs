{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module RecoveryModule.Tests where

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

import           RecoveryModule

tests :: TestTree
tests = testGroup "Recovery Module Tests"
  [ testCase "Request recovery" testRequestRecovery
  , testCase "Approve recovery" testApproveRecovery
  , testCase "Execute recovery" testExecuteRecovery
  , testCase "Execute recovery with insufficient approvals fails" testExecuteRecoveryInsufficientApprovals
  , testCase "Execute already executed recovery fails" testExecuteAlreadyExecutedRecovery
  ]

-- Test data
testOwner :: PubKeyHash
testOwner = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

testGuardian1 :: PubKeyHash
testGuardian1 = "1123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

testGuardian2 :: PubKeyHash
testGuardian2 = "2123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

testTransactionId :: BuiltinByteString
testTransactionId = "tx-123"

testReason :: BuiltinByteString
testReason = "wrong-amount"

testTimestamp :: POSIXTime
testTimestamp = 1_000_000

testRecoveryId :: BuiltinByteString
testRecoveryId = "recovery-123"

testRecoveryRequest :: RecoveryRequest
testRecoveryRequest = RecoveryRequest
  { requestOwner = testOwner
  , transactionId = testTransactionId
  , requestReason = testReason
  , requestTimestamp = testTimestamp
  , recoveryAmount = 100_000_000
  , recoveryToken = AssetClass (adaSymbol, adaToken)
  , recoveryAddress = testOwner
  }

testApproval1 :: GuardianApproval
testApproval1 = GuardianApproval
  { guardian = testGuardian1
  , approvalStatus = True
  , approvalTimestamp = 1_100_000
  }

testApproval2 :: GuardianApproval
testApproval2 = GuardianApproval
  { guardian = testGuardian2
  , approvalStatus = True
  , approvalTimestamp = 1_200_000
  }

testRecoveryDatum :: RecoveryDatum
testRecoveryDatum = RecoveryDatum
  { recoveryId = testRecoveryId
  , request = testRecoveryRequest
  , approvals = []
  , isExecuted = False
  , requiredApprovals = 2
  }

testRecoveryDatumWithApprovals :: RecoveryDatum
testRecoveryDatumWithApprovals = testRecoveryDatum
  { approvals = [testApproval1, testApproval2]
  }

-- Test requesting recovery
testRequestRecovery :: IO ()
testRequestRecovery = do
  let validator = mkRecoveryValidator
      redeemer = RequestRecovery testRecoveryRequest
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Request recovery should succeed" (validator testRecoveryDatum redeemer context)

-- Test approving recovery
testApproveRecovery :: IO ()
testApproveRecovery = do
  let validator = mkRecoveryValidator
      redeemer = ApproveRecovery testRecoveryId testApproval1
      context = scriptContextMock testGuardian1 1_500_000
  
  assertBool "Approve recovery should succeed" (validator testRecoveryDatum redeemer context)

-- Test executing recovery
testExecuteRecovery :: IO ()
testExecuteRecovery = do
  let validator = mkRecoveryValidator
      redeemer = ExecuteRecovery testRecoveryId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Execute recovery should succeed" (validator testRecoveryDatumWithApprovals redeemer context)

-- Test executing recovery with insufficient approvals
testExecuteRecoveryInsufficientApprovals :: IO ()
testExecuteRecoveryInsufficientApprovals = do
  let datumWithOneApproval = testRecoveryDatum { approvals = [testApproval1] }
      validator = mkRecoveryValidator
      redeemer = ExecuteRecovery testRecoveryId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Execute recovery with insufficient approvals should fail" 
    (not $ validator datumWithOneApproval redeemer context)

-- Test executing an already executed recovery
testExecuteAlreadyExecutedRecovery :: IO ()
testExecuteAlreadyExecutedRecovery = do
  let executedDatum = testRecoveryDatumWithApprovals { isExecuted = True }
      validator = mkRecoveryValidator
      redeemer = ExecuteRecovery testRecoveryId
      context = scriptContextMock testOwner 1_500_000
  
  assertBool "Execute already executed recovery should fail" 
    (not $ validator executedDatum redeemer context)

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