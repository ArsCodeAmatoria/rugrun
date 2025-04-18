{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           System.Directory     (createDirectoryIfMissing)
import           System.Environment   (getArgs)
import           System.FilePath      ((</>))
import           System.IO            (hPutStrLn, stderr)
import           Prelude              hiding (putStrLn)
import qualified Prelude              as P
import           Data.List            (intercalate)

import           WalletCompiler
import qualified RugRun.Types as Types
import qualified RugRun.Validators as Validators

putStrLn :: String -> IO ()
putStrLn = P.putStrLn

-- | Main entry point
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compile", outputDir, secretPhrase] -> do
            compileWalletValidator outputDir secretPhrase
            putStrLn $ "Compiled validator scripts for secret: " ++ secretPhrase

        ["generate-hash", secretPhrase] -> do
            let hash = generateSecretHash secretPhrase
            putStrLn $ "Generated hash for secret '" ++ secretPhrase ++ "': " ++ hash

        ["create-game-wallets", outputDir, realSecret, outFile] -> do
            -- Generate some decoy secrets
            let decoySecrets = 
                    [ "monoid"
                    , "functor"
                    , "monad"
                    , "applicative"
                    , "recursion"
                    ]
            
            putStrLn $ "Creating game wallets with real secret: " ++ realSecret
            putStrLn $ "Decoy secrets: " ++ intercalate ", " decoySecrets
            
            -- Create wallets
            wallets <- createGameWallets outputDir realSecret decoySecrets
            
            -- Write wallet data to JSON file
            createDirectoryIfMissing True outputDir
            BSL.writeFile outFile (Aeson.encode wallets)
            
            putStrLn $ "Created " ++ show (length wallets) ++ " wallets"
            putStrLn $ "Wallet data written to: " ++ outFile
        
        ["test-validator", secretHash, testSecret] -> do
            -- Test if a secret validates against a hash
            let result = validateSecret (read secretHash) (read testSecret)
            putStrLn $ "Validating secret '" ++ testSecret ++ "' against hash '" ++ secretHash ++ "'"
            putStrLn $ "Result: " ++ (if result then "VALID!" else "INVALID!")
            
        ["compile-validators", outputDir] -> do
            -- Compile both rug and decoy wallet validators
            Validators.writeRugWalletValidator (outputDir </> "rug-wallet.plutus")
            Validators.writeDecoyWalletValidator (outputDir </> "decoy-wallet.plutus")
            putStrLn $ "Compiled both validators to: " ++ outputDir
        
        ["generate-game", outputDir, realSecret, walletCount, outFile] -> do
            let count = read walletCount
            
            -- Generate realistic decoy secrets
            let decoySecrets = 
                    [ "monoid", "functor", "monad", "applicative", "recursion",
                      "cardano", "plutus", "marlowe", "blockchain", "transaction",
                      "smart_contract", "token", "NFT", "staking", "delegation"
                    ]
            
            putStrLn $ "Creating game with " ++ show count ++ " wallets"
            putStrLn $ "Real wallet secret: " ++ realSecret
            
            -- Ensure enough decoy secrets
            let requiredDecoys = count - 1
                actualDecoys = take requiredDecoys decoySecrets
                
            if length actualDecoys < requiredDecoys
                then putStrLn "Warning: Not enough decoy secrets, some will be duplicated"
                else pure ()
            
            -- Create enough decoys (with repeats if needed)
            let finalDecoys = take requiredDecoys (cycle decoySecrets)
                
            -- Create wallets
            wallets <- createGameWallets outputDir realSecret finalDecoys
            
            -- Write wallet data to JSON file
            createDirectoryIfMissing True outputDir
            BSL.writeFile outFile (Aeson.encode wallets)
            
            putStrLn $ "Created " ++ show (length wallets) ++ " wallets"
            putStrLn $ "Wallet data written to: " ++ outFile

        _ -> do
            hPutStrLn stderr "Usage:"
            hPutStrLn stderr "  compile <output-dir> <secret-phrase>"
            hPutStrLn stderr "  generate-hash <secret-phrase>"
            hPutStrLn stderr "  create-game-wallets <output-dir> <real-secret> <output-file>"
            hPutStrLn stderr "  test-validator <secret-hash> <test-secret>"
            hPutStrLn stderr "  compile-validators <output-dir>"
            hPutStrLn stderr "  generate-game <output-dir> <real-secret> <wallet-count> <output-file>" 