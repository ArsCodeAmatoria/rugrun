{-# LANGUAGE OverloadedStrings #-}

module WalletCompiler
    ( compileWalletValidator
    , generateSecretHash
    , createWalletScript
    , createGameWallets
    , GameWallet(..)
    , WalletType(..)
    ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Aeson           (ToJSON(..), FromJSON(..), object, (.=), (.:))
import qualified Data.Aeson           as Aeson
import           System.Random        (randomRIO)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           GHC.Generics         (Generic)
import           RugRun.Validators    (writeRugWalletValidator, writeDecoyWalletValidator, validateSecret)
import qualified RugRun.Types         as Types
import qualified Prelude              as Haskell
import           Prelude              (IO, String, FilePath, Bool(..), ($), (++), (<$>), mapM_, return, fmap, show)
import qualified PlutusTx.Prelude     as PlutusTx
import qualified Data.ByteString.Lazy as LBS

-- | Type of wallet (real or decoy)
data WalletType = RealWallet | DecoyWallet 
    deriving (Haskell.Eq, Haskell.Show, Generic)

instance ToJSON WalletType where
    toJSON RealWallet = Aeson.String "real"
    toJSON DecoyWallet = Aeson.String "decoy"

instance FromJSON WalletType where
    parseJSON (Aeson.String "real") = return RealWallet
    parseJSON (Aeson.String "decoy") = return DecoyWallet
    parseJSON _ = fail "Invalid wallet type"

-- | Game wallet data structure
data GameWallet = GameWallet
    { walletAddress    :: String      -- ^ Wallet address
    , walletBalance    :: Integer     -- ^ Wallet ADA balance
    , walletSecretHash :: String      -- ^ Hash of the secret phrase
    , walletType       :: WalletType  -- ^ Type of wallet
    , walletClue       :: String      -- ^ Clue for the secret phrase
    } deriving (Haskell.Show, Generic)

instance ToJSON GameWallet where
    toJSON wallet = object
        [ "address" .= walletAddress wallet
        , "balance" .= walletBalance wallet 
        , "secretHash" .= walletSecretHash wallet
        , "type" .= walletType wallet
        , "clue" .= walletClue wallet
        ]

instance FromJSON GameWallet where
    parseJSON = Aeson.withObject "GameWallet" $ \v -> GameWallet
        <$> v .: "address"
        <$> v .: "balance"
        <$> v .: "secretHash"
        <$> v .: "type"
        <$> v .: "clue"

-- | Convert a string to a BuiltinByteString
stringToBuiltinBS :: String -> PlutusTx.BuiltinByteString
stringToBuiltinBS = PlutusTx.toBuiltin . TE.encodeUtf8 . T.pack

-- | Generate a SHA-256 hash from a secret phrase
generateSecretHash :: String -> String
generateSecretHash secret =
    let secretBS = stringToBuiltinBS secret
        hashBS = PlutusTx.sha2_256 secretBS
    in BSC.unpack $ PlutusTx.fromBuiltin hashBS

-- | Generate a file name from a secret phrase (simplified, just first 8 chars of hash)
generateScriptFileName :: String -> String
generateScriptFileName secret = 
    let hash = generateSecretHash secret
    in take 8 hash

-- | Compile the wallet validator script with a specific secret phrase
compileWalletValidator :: FilePath -> String -> IO ()
compileWalletValidator outputDir secretPhrase = do
    let scriptName = "wallet-validator-" ++ generateScriptFileName secretPhrase ++ ".plutus"
        outputPath = outputDir </> scriptName
    
    createDirectoryIfMissing True outputDir
    writeRugWalletValidator outputPath
    Haskell.putStrLn $ "Compiled rug wallet validator: " ++ outputPath
    
    let decoyName = "decoy-validator-" ++ generateScriptFileName secretPhrase ++ ".plutus"
        decoyPath = outputDir </> decoyName
    
    writeDecoyWalletValidator decoyPath
    Haskell.putStrLn $ "Compiled decoy wallet validator: " ++ decoyPath

-- | Create a wallet script for a specific secret
createWalletScript :: FilePath -> String -> IO FilePath
createWalletScript outputDir secretPhrase = do
    let scriptName = 
            if isRealWallet secretPhrase 
            then "wallet-validator-" ++ generateScriptFileName secretPhrase ++ ".plutus"
            else "decoy-validator-" ++ generateScriptFileName secretPhrase ++ ".plutus"
        outputPath = outputDir </> scriptName
    
    createDirectoryIfMissing True outputDir
    
    if isRealWallet secretPhrase
    then writeRugWalletValidator outputPath
    else writeDecoyWalletValidator outputPath
    
    return outputPath

-- | Check if a secret phrase is for a real wallet (simple convention for testing)
isRealWallet :: String -> Bool
isRealWallet secret = "real" `Haskell.isPrefixOf` secret || "haskell" `Haskell.isPrefixOf` secret

-- | Generate a list of clues for the game
generateClues :: IO [String]
generateClues = return
    [ "The secret is a famous functional programming concept."
    , "The secret is related to lambda calculus."
    , "The secret is a Haskell compiler."
    , "The secret is a data structure used in functional programming."
    , "The secret is a mathematical concept used in computer science."
    , "The secret involves recursion."
    , "The secret is a pattern matching technique."
    , "The secret is a type of monad."
    , "The secret is a Cardano concept."
    , "The secret is related to smart contracts."
    ]

-- | Generate random ADA amount (between 100 and 1000 ADA)
generateRandomBalance :: IO Integer
generateRandomBalance = randomRIO (100, 1000)

-- | Create multiple wallets for a game
createGameWallets :: FilePath -> String -> [String] -> IO [GameWallet]
createGameWallets outputDir realSecret decoySecrets = do
    -- Generate clues
    allClues <- generateClues
    
    -- Create the real wallet
    realBalance <- generateRandomBalance
    let realHash = generateSecretHash realSecret
        realClue = head allClues
    
    realWallet <- createWallet outputDir realSecret realBalance RealWallet realClue
    
    -- Create decoy wallets
    let remainingClues = tail allClues
    decoyWallets <- Haskell.zipWithM 
                     (\secret clue -> do
                         balance <- generateRandomBalance
                         createWallet outputDir secret balance DecoyWallet clue) 
                     decoySecrets 
                     remainingClues
    
    return (realWallet : decoyWallets)

-- | Create a single wallet
createWallet :: FilePath -> String -> Integer -> WalletType -> String -> IO GameWallet
createWallet outputDir secret balance walletType clue = do
    scriptPath <- createWalletScript outputDir secret
    -- In a real implementation, this would generate an actual address
    -- For now, we'll use a placeholder that includes the script path
    let address = "addr_" ++ generateScriptFileName secret
        secretHash = generateSecretHash secret
    
    return $ GameWallet
        { walletAddress = address
        , walletBalance = balance
        , walletSecretHash = secretHash
        , walletType = walletType
        , walletClue = clue
        } 