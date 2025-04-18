module Main where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString            as BS
import qualified Data.Aeson                 as JSON
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))
import           System.Directory           (createDirectoryIfMissing)
import           Control.Monad              (forM_, when)

import           RugRun.Contract            (rugWalletScript, decoyWalletScript, 
                                           rugWalletHash, decoyWalletHash)
import           RugRun.Types
import           RugRun.Utils               (sha256, verifySecret)

import           Prelude
import           Plutus.V1.Ledger.Api       (BuiltinByteString)
import qualified PlutusTx.Builtins          as Builtins

-- | Output directories
outputDir :: FilePath
outputDir = "output"

scriptsDir :: FilePath
scriptsDir = outputDir </> "scripts"

-- | Export the validator scripts to files in multiple formats
exportScripts :: IO ()
exportScripts = do
  -- Create output directories
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True scriptsDir
  
  -- Export in Plutus format
  LBS.writeFile (scriptsDir </> "rugWallet.plutus") $ JSON.encode rugWalletScript
  LBS.writeFile (scriptsDir </> "decoyWallet.plutus") $ JSON.encode decoyWalletScript
  
  -- Export hash values
  writeFile (scriptsDir </> "rugWalletHash.txt") (show rugWalletHash)
  writeFile (scriptsDir </> "decoyWalletHash.txt") (show decoyWalletHash)
  
  -- Export as JSON for the frontend
  let scriptsJSON = JSON.object [ 
          "rugWalletScript" JSON..= rugWalletScript
        , "decoyWalletScript" JSON..= decoyWalletScript
        , "rugWalletHash" JSON..= show rugWalletHash
        , "decoyWalletHash" JSON..= show decoyWalletHash
        ]
  
  LBS.writeFile (outputDir </> "contracts.json") $ JSON.encode scriptsJSON
  
  putStrLn "Scripts exported to the output directory:"
  putStrLn $ "- " ++ scriptsDir </> "rugWallet.plutus"
  putStrLn $ "- " ++ scriptsDir </> "decoyWallet.plutus"
  putStrLn $ "- " ++ scriptsDir </> "rugWalletHash.txt"
  putStrLn $ "- " ++ scriptsDir </> "decoyWalletHash.txt"
  putStrLn $ "- " ++ outputDir </> "contracts.json"

-- | Test secret hash verification
testSecretVerification :: String -> String -> IO ()
testSecretVerification secretStr expectedHashStr = do
  let secret = Builtins.toBuiltin $ TE.encodeUtf8 $ T.pack secretStr
      expectedHash = Builtins.toBuiltin $ TE.encodeUtf8 $ T.pack expectedHashStr
      computedHash = sha256 secret
      verified = verifySecret expectedHash secret
      
  putStrLn $ "Secret: " ++ secretStr
  putStrLn $ "Expected hash: " ++ expectedHashStr
  putStrLn $ "Computed hash: " ++ T.unpack (TE.decodeUtf8 $ Builtins.fromBuiltin computedHash)
  putStrLn $ "Verification result: " ++ if verified then "VALID" else "INVALID"
  
  if verified 
    then exitSuccess 
    else exitFailure

-- | Display usage information
printUsage :: IO ()
printUsage = do
  putStrLn "RugRun Cardano Contract Tool"
  putStrLn "============================"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  rugrun export                     -- Export all validator scripts"
  putStrLn "  rugrun verify SECRET HASH         -- Test secret verification"
  putStrLn "  rugrun hash SECRET                -- Compute hash of a secret"
  putStrLn "  rugrun help                       -- Show this help message"

-- | Compute hash of a secret phrase
hashSecret :: String -> IO ()
hashSecret secretStr = do
  let secret = Builtins.toBuiltin $ TE.encodeUtf8 $ T.pack secretStr
      computedHash = sha256 secret
      
  putStrLn $ "Secret: " ++ secretStr
  putStrLn $ "Hash: " ++ T.unpack (TE.decodeUtf8 $ Builtins.fromBuiltin computedHash)

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export"] -> exportScripts
    
    ["verify", secret, hash] -> testSecretVerification secret hash
    
    ["hash", secret] -> hashSecret secret
    
    ["help"] -> printUsage
    
    _ -> do
      putStrLn "Invalid command. Use 'rugrun help' for usage information."
      printUsage 