--- cardano/plutus/src/RecoveryModule.hs
+++ cardano/plutus/src/RecoveryModule.hs
@@ -0,0 +1,120 @@
+{-# LANGUAGE DataKinds           #-}
+{-# LANGUAGE FlexibleContexts    #-}
+{-# LANGUAGE NoImplicitPrelude   #-}
+{-# LANGUAGE ScopedTypeVariables #-}
+{-# LANGUAGE TemplateHaskell     #-}
+{-# LANGUAGE TypeApplications    #-}
+{-# LANGUAGE TypeFamilies        #-}
+{-# LANGUAGE TypeOperators       #-}
+
+module RecoveryModule where
+
+import           PlutusTx.Prelude
+import qualified PlutusTx
+import           Plutus.V2.Ledger.Api
+import           Plutus.V2.Ledger.Contexts
+import qualified Ledger.Typed.Scripts as Scripts
+import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
+import           Ledger.Ada           as Ada
+import           Plutus.Script.Utils.V2.Scripts (scriptHash)
+
+-- | Recovery request data structure
+data RecoveryRequest = RecoveryRequest
+    { requestOwner    :: PubKeyHash
+    , transactionId   :: BuiltinByteString
+    , requestReason   :: BuiltinByteString
+    , requestTimestamp :: POSIXTime
+    , recoveryAmount  :: Integer
+    , recoveryToken   :: AssetClass
+    , recoveryAddress :: PubKeyHash
+    }
+
+PlutusTx.unstableMakeIsData ''RecoveryRequest
+
+-- | Guardian approval data structure
+data GuardianApproval = GuardianApproval
+    { guardian        :: PubKeyHash
+    , approvalStatus  :: Bool
+    , approvalTimestamp :: POSIXTime
+    }
+
+PlutusTx.unstableMakeIsData ''GuardianApproval
+
+-- | Datum for the recovery module
+data RecoveryDatum = RecoveryDatum
+    { recoveryId      :: BuiltinByteString
+    , request         :: RecoveryRequest
+    , approvals       :: [GuardianApproval]
+    , isExecuted      :: Bool
+    , requiredApprovals :: Integer
+    }
+
+PlutusTx.unstableMakeIsData ''RecoveryDatum
+
+-- | Redeemer for the recovery module
+data RecoveryRedeemer = 
+      RequestRecovery RecoveryRequest
+    | ApproveRecovery BuiltinByteString GuardianApproval
+    | ExecuteRecovery BuiltinByteString
+    | CancelRecovery BuiltinByteString
+
+PlutusTx.unstableMakeIsData ''RecoveryRedeemer
+
+{-# INLINABLE countApprovals #-}
+countApprovals :: [GuardianApproval] -> Integer
+countApprovals = foldr (\approval acc -> if approvalStatus approval then acc + 1 else acc) 0
+
+{-# INLINABLE mkRecoveryValidator #-}
+mkRecoveryValidator :: RecoveryDatum -> RecoveryRedeemer -> ScriptContext -> Bool
+mkRecoveryValidator datum redeemer ctx = case redeemer of
+    RequestRecovery request' -> 
+        -- Check that the request owner is signing the transaction
+        traceIfFalse "Request owner must sign" (txSignedBy info (requestOwner request'))
+        
+    ApproveRecovery recoveryId' approval ->
+        -- Check that the recovery ID matches
+        traceIfFalse "Recovery ID mismatch" (recoveryId datum == recoveryId') &&
+        -- Check that the recovery is not executed
+        traceIfFalse "Recovery already executed" (not $ isExecuted datum) &&
+        -- Check that the guardian is signing the transaction
+        traceIfFalse "Guardian must sign" (txSignedBy info (guardian approval))
+        
+    ExecuteRecovery recoveryId' ->
+        -- Check that the recovery ID matches
+        traceIfFalse "Recovery ID mismatch" (recoveryId datum == recoveryId') &&
+        -- Check that the recovery is not executed
+        traceIfFalse "Recovery already executed" (not $ isExecuted datum) &&
+        -- Check that there are enough approvals
+        traceIfFalse "Not enough approvals" (countApprovals (approvals datum) >= requiredApprovals datum)
+        
+    CancelRecovery recoveryId' ->
+        -- Check that the recovery ID matches
+        traceIfFalse "Recovery ID mismatch" (recoveryId datum == recoveryId') &&
+        -- Check that the recovery is not executed
+        traceIfFalse "Recovery already executed" (not $ isExecuted datum) &&
+        -- Check that the request owner is signing the transaction
+        traceIfFalse "Request owner must sign" (txSignedBy info (requestOwner (request datum)))
+  where
+    info :: TxInfo
+    info = scriptContextTxInfo ctx
+
+data RecoveryModule
+instance Scripts.ValidatorTypes RecoveryModule where
+    type instance DatumType RecoveryModule = RecoveryDatum
+    type instance RedeemerType RecoveryModule = RecoveryRedeemer
+
+typedValidator :: Scripts.TypedValidator RecoveryModule
+typedValidator = Scripts.mkTypedValidator @RecoveryModule
+    $$(PlutusTx.compile [|| mkRecoveryValidator ||])
+    $$(PlutusTx.compile [|| wrap ||])
+  where
+    wrap = Scripts.wrapValidator @RecoveryDatum @RecoveryRedeemer
+
+validator :: Validator
+validator = Scripts.validatorScript typedValidator
+
+valHash :: ValidatorHash
+valHash = Scripts.validatorHash typedValidator
+
+scrAddress :: Address
+scrAddress = scriptHashAddress valHash