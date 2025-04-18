module Main where

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Aeson             as JSON
import           System.Environment     (getArgs)
import           RugRun.Contract        (rugWalletScript, decoyWalletScript)

-- | Export the validator scripts to files
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["export"] -> do
      LBS.writeFile "rugWallet.plutus" $ JSON.encode rugWalletScript
      LBS.writeFile "decoyWallet.plutus" $ JSON.encode decoyWalletScript
      putStrLn "Scripts exported to rugWallet.plutus and decoyWallet.plutus"
    _ -> putStrLn "Usage: rugrun export" 