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
  , toHexString
  , fromHexString
  , isValidWalletAddress
  , calculateReward
  , validateTimeRange
  ) where

import           PlutusTx.Prelude
import qualified Prelude                     as Haskell
import           Plutus.V1.Ledger.Api        (BuiltinByteString, POSIXTime, Value, AssetClass, assetClassValueOf, LowerBound, UpperBound, Interval(..))
import qualified PlutusTx.Builtins           as Builtins
import           Plutus.V1.Ledger.Interval   (contains, from, to, strictUpperBound, strictLowerBound, Closure)
import           Plutus.V1.Ledger.Contexts   (ScriptContext, TxInfo, scriptContextTxInfo, valuePaidTo, findOwnInput, findDatum, txSignedBy, txInfoValidRange)

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
calculateFee totalValue feePercentage = 
  (totalValue * feePercentage) `divide` 100

-- | Encode a clue to make it slightly obfuscated
-- This is a simple XOR with a fixed key for demonstration
encodeClue :: BuiltinByteString -> BuiltinByteString
encodeClue clue =
  let 
    key = stringToBuiltinByteString "RUGRUN"
    keyLength = lengthOfByteString key
    
    applyXor :: Integer -> BuiltinByteString -> BuiltinByteString
    applyXor i bs
      | i >= lengthOfByteString clue = bs
      | otherwise =
          let 
            keyIndex = i `modulo` keyLength
            keyByte = indexByteString key keyIndex
            clueByte = indexByteString clue i
            resultByte = keyByte `xor` clueByte
          in
            applyXor (i + 1) (consByteString resultByte bs)
  in
    applyXor 0 emptyByteString

-- | Decode an encoded clue
decodeClue :: BuiltinByteString -> BuiltinByteString
decodeClue = encodeClue  -- XOR is symmetric, so encoding again will decode it

-- | Convert a ByteString to a hexadecimal representation
toHexString :: BuiltinByteString -> BuiltinByteString
toHexString bs =
  let
    -- Convert a single byte to two hex characters
    byteToHex :: Integer -> BuiltinByteString
    byteToHex b = 
      let
        highNibble = b `divide` 16
        lowNibble = b `modulo` 16
        toHexChar n
          | n < 10 = n + 48  -- '0'-'9' (ASCII 48-57)
          | otherwise = n + 87  -- 'a'-'f' (ASCII 97-102)
      in
        consByteString (toHexChar highNibble) (consByteString (toHexChar lowNibble) emptyByteString)
        
    -- Process each byte in the input
    processBytes :: Integer -> BuiltinByteString -> BuiltinByteString
    processBytes i acc
      | i >= lengthOfByteString bs = acc
      | otherwise = 
          let 
            byte = indexByteString bs i
            hexChars = byteToHex byte
          in
            processBytes (i + 1) (appendByteString acc hexChars)
  in
    processBytes 0 emptyByteString

-- | Convert a hex string back to bytes (partial implementation)
fromHexString :: BuiltinByteString -> BuiltinByteString
fromHexString hexStr =
  let
    -- Convert two hex characters to a single byte
    hexToByte :: Integer -> Integer -> Integer
    hexToByte high low =
      let
        fromHexChar c
          | c >= 48 && c <= 57 = c - 48  -- '0'-'9'
          | c >= 97 && c <= 102 = c - 87  -- 'a'-'f'
          | c >= 65 && c <= 70 = c - 55   -- 'A'-'F'
          | otherwise = 0
      in
        (fromHexChar high * 16) + fromHexChar low
        
    -- Process hex string two characters at a time
    processHex :: Integer -> BuiltinByteString -> BuiltinByteString
    processHex i acc
      | i >= lengthOfByteString hexStr - 1 = acc
      | otherwise = 
          let 
            highChar = indexByteString hexStr i
            lowChar = indexByteString hexStr (i + 1)
            byte = hexToByte highChar lowChar
          in
            processHex (i + 2) (consByteString byte acc)
  in
    processHex 0 emptyByteString

-- | Simple wallet address validation (prefix check)
isValidWalletAddress :: BuiltinByteString -> Bool
isValidWalletAddress addr =
  let
    -- Check if address starts with a valid prefix (simplified example)
    -- In a real implementation, this would do proper validation based on address format
    validPrefixes = [stringToBuiltinByteString "addr1", stringToBuiltinByteString "addr_test1"]
    
    hasPrefix prefix =
      let
        prefixLen = lengthOfByteString prefix
        addrPrefix = sliceByteString 0 prefixLen addr
      in
        addrPrefix == prefix
        
    checkPrefixes [] = False
    checkPrefixes (p:ps) = if hasPrefix p then True else checkPrefixes ps
  in
    checkPrefixes validPrefixes && lengthOfByteString addr >= 20

-- | Calculate reward based on attempt count and wallet value
calculateReward :: Integer -> Integer -> Integer -> Integer
calculateReward baseValue attemptCount maxAttempts =
  let
    -- The fewer attempts used, the higher the reward
    attemptsRemaining = maxAttempts - attemptCount
    bonusPercentage = (attemptsRemaining * 100) `divide` maxAttempts
    bonusValue = (baseValue * bonusPercentage) `divide` 100
  in
    baseValue + (if attemptCount < maxAttempts then bonusValue else 0)

-- | Validate a time range is within expected bounds
validateTimeRange :: Interval POSIXTime -> POSIXTime -> POSIXTime -> Bool
validateTimeRange range minTime maxTime =
  let
    minBound = strictLowerBound minTime
    maxBound = strictUpperBound maxTime
    validMinBound = case ivFrom range of
      LowerBound t _ -> t >= minBound
      _ -> False
    validMaxBound = case ivTo range of
      UpperBound t _ -> t <= maxBound
      _ -> False
  in
    validMinBound && validMaxBound

-- | Helper to convert a String to BuiltinByteString
stringToBuiltinByteString :: Haskell.String -> BuiltinByteString
stringToBuiltinByteString = Builtins.toBuiltin 