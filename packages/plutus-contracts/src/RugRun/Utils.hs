{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RugRun.Utils
  ( sha256
  , emitEvent
  , verifySecret
  , generateGameId
  , validateExpiration
  , getWalletBalance
  , encodeClue
  , decodeClue
  , calculateFee
  ) where

import           PlutusTx.Prelude
import qualified Prelude                     as Haskell
import           Plutus.V1.Ledger.Api        (BuiltinByteString, POSIXTime, Value, AssetClass, assetClassValueOf)
import qualified PlutusTx.Builtins           as Builtins
import           Plutus.V1.Ledger.Interval   (contains, from)

-- | Compute SHA-256 hash of input
sha256 :: BuiltinByteString -> BuiltinByteString
sha256 = Builtins.sha2_256

-- | Verify if a provided secret phrase matches the expected hash
verifySecret :: BuiltinByteString -> BuiltinByteString -> Bool
verifySecret secretHash providedSecret = sha256 providedSecret == secretHash

-- | Emit an on-chain event (this is a placeholder - actual implementation depends on the chain)
emitEvent :: BuiltinByteString -> BuiltinByteString -> Builtins.BuiltinData
emitEvent eventName eventData = Builtins.mkI 0  -- Placeholder implementation

-- | Generate a unique game ID based on seed and timestamp
generateGameId :: BuiltinByteString -> POSIXTime -> BuiltinByteString
generateGameId seed timestamp = 
  let 
    timeBytes = Builtins.toBuiltin $ Builtins.indexByteString (Builtins.consByteString timestamp emptyByteString) 0
    combined = appendByteString seed timeBytes
  in 
    sha256 combined

-- | Check if wallet has expired based on current time
validateExpiration :: POSIXTime -> POSIXTime -> Bool
validateExpiration currentTime expirationTime = 
  from expirationTime `contains` currentTime

-- | Get the balance of a specific token in a wallet
getWalletBalance :: Value -> AssetClass -> Integer
getWalletBalance = assetClassValueOf

-- | Calculate fee amount based on total value and percentage
calculateFee :: Integer -> Integer -> Integer
calculateFee totalAmount feePercentage =
  divide (totalAmount * feePercentage) 100

-- | Encode a clue string with simple substitution (for adding obfuscation)
encodeClue :: BuiltinByteString -> BuiltinByteString
encodeClue clue =
  let
    encoder :: Integer -> Integer
    encoder c = (c + 7) `modulo` 256
  in
    mapByteString encoder clue

-- | Decode an encoded clue string
decodeClue :: BuiltinByteString -> BuiltinByteString
decodeClue encodedClue =
  let
    decoder :: Integer -> Integer
    decoder c = (c + 249) `modulo` 256  -- (c - 7) mod 256, but we use +249 to avoid negative
  in
    mapByteString decoder encodedClue

-- | Helper to map a function over each byte in a ByteString
mapByteString :: (Integer -> Integer) -> BuiltinByteString -> BuiltinByteString
mapByteString f bs =
  let
    len = lengthOfByteString bs
    
    go :: Integer -> BuiltinByteString -> BuiltinByteString
    go i acc
      | i >= len = acc
      | otherwise =
          let byte = indexByteString bs i
              newByte = f byte
          in go (i + 1) (consByteString newByte acc)
  in
    go 0 emptyByteString 