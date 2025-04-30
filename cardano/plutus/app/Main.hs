--- cardano/plutus/app/Main.hs
+++ cardano/plutus/app/Main.hs
@@ -0,0 +1,39 @@
+module Main where
+
+import           Cardano.Api
+import           Cardano.Api.Shelley   (PlutusScript (..))
+import           Codec.Serialise       (serialise)
+import qualified Data.ByteString.Lazy  as LBS
+import qualified Data.ByteString.Short as SBS
+import           System.Environment    (getArgs)
+import           System.FilePath       ((</>))
+
+import qualified IntentRegistry        as IR
+import qualified RecoveryModule        as RM
+
+-- | Convert a Plutus script to a Cardano API PlutusScript
+plutusScriptToCardanoAPI :: Plutus.V2.Ledger.Api.Script -> PlutusScript PlutusScriptV2
+plutusScriptToCardanoAPI script = PlutusScriptSerialised $ SBS.toShort $ LBS.toStrict $ serialise script
+
+-- | Write a Plutus script to a file
+writeScript :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO ()
+writeScript filePath validator = do
+    let scriptBytes = serialiseToRawBytes $ plutusScriptToCardanoAPI $ Plutus.V2.Ledger.Api.unValidatorScript validator
+    case scriptBytes of
+        Left err -> putStrLn $ "Error: " ++ show err
+        Right bytes -> writeFileRaw filePath bytes
+
+main :: IO ()
+main = do
+    args <- getArgs
+    let outputDir = if null args then "scripts" else head args
+    
+    -- Write Intent Registry script
+    putStrLn "Compiling Intent Registry script..."
+    writeScript (outputDir </> "intent-registry.plutus") IR.validator
+    
+    -- Write Recovery Module script
+    putStrLn "Compiling Recovery Module script..."
+    writeScript (outputDir </> "recovery-module.plutus") RM.validator
+    
+    putStrLn "Scripts compiled successfully!"