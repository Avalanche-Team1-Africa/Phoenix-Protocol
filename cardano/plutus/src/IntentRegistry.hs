--- cardano/plutus/src/IntentRegistry.hs
+++ cardano/plutus/src/IntentRegistry.hs
@@ -0,0 +1,102 @@
+{-# LANGUAGE DataKinds           #-}
+{-# LANGUAGE FlexibleContexts    #-}
+{-# LANGUAGE NoImplicitPrelude   #-}
+{-# LANGUAGE ScopedTypeVariables #-}
+{-# LANGUAGE TemplateHaskell     #-}
+{-# LANGUAGE TypeApplications    #-}
+{-# LANGUAGE TypeFamilies        #-}
+{-# LANGUAGE TypeOperators       #-}
+
+module IntentRegistry where
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
+-- | Intent data structure
+data Intent = Intent
+    { intentOwner      :: PubKeyHash
+    , intentAction     :: BuiltinByteString
+    , intentParameters :: BuiltinByteString
+    , intentTimestamp  :: POSIXTime
+    , intentExpiry     :: POSIXTime
+    }
+
+PlutusTx.unstableMakeIsData ''Intent
+
+-- | Datum for the intent registry
+data IntentDatum = IntentDatum
+    { intentId    :: BuiltinByteString
+    , intent      :: Intent
+    , isExecuted  :: Bool
+    }
+
+PlutusTx.unstableMakeIsData ''IntentDatum
+
+-- | Redeemer for the intent registry
+data IntentRedeemer = 
+      RegisterIntent Intent
+    | ExecuteIntent BuiltinByteString
+    | CancelIntent BuiltinByteString
+
+PlutusTx.unstableMakeIsData ''IntentRedeemer
+
+{-# INLINABLE mkIntentValidator #-}
+mkIntentValidator :: IntentDatum -> IntentRedeemer -> ScriptContext -> Bool
+mkIntentValidator datum redeemer ctx = case redeemer of
+    RegisterIntent intent' -> 
+        -- Check that the intent owner is signing the transaction
+        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner intent')) &&
+        -- Check that the intent is not expired
+        traceIfFalse "Intent is expired" (intentExpiry intent' > currentTime)
+        
+    ExecuteIntent intentId' ->
+        -- Check that the intent ID matches
+        traceIfFalse "Intent ID mismatch" (intentId datum == intentId') &&
+        -- Check that the intent is not executed
+        traceIfFalse "Intent already executed" (not $ isExecuted datum) &&
+        -- Check that the intent is not expired
+        traceIfFalse "Intent is expired" (intentExpiry (intent datum) > currentTime) &&
+        -- Check that the intent owner is signing the transaction
+        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner (intent datum)))
+        
+    CancelIntent intentId' ->
+        -- Check that the intent ID matches
+        traceIfFalse "Intent ID mismatch" (intentId datum == intentId') &&
+        -- Check that the intent is not executed
+        traceIfFalse "Intent already executed" (not $ isExecuted datum) &&
+        -- Check that the intent owner is signing the transaction
+        traceIfFalse "Intent owner must sign" (txSignedBy info (intentOwner (intent datum)))
+  where
+    info :: TxInfo
+    info = scriptContextTxInfo ctx
+    
+    currentTime :: POSIXTime
+    currentTime = case txInfoValidRange info of
+        (from, _) -> from
+
+data IntentRegistry
+instance Scripts.ValidatorTypes IntentRegistry where
+    type instance DatumType IntentRegistry = IntentDatum
+    type instance RedeemerType IntentRegistry = IntentRedeemer
+
+typedValidator :: Scripts.TypedValidator IntentRegistry
+typedValidator = Scripts.mkTypedValidator @IntentRegistry
+    $$(PlutusTx.compile [|| mkIntentValidator ||])
+    $$(PlutusTx.compile [|| wrap ||])
+  where
+    wrap = Scripts.wrapValidator @IntentDatum @IntentRedeemer
+
+validator :: Validator
+validator = Scripts.validatorScript typedValidator
+
+valHash :: ValidatorHash
+valHash = Scripts.validatorHash typedValidator
+
+scrAddress :: Address
+scrAddress = scriptHashAddress valHash