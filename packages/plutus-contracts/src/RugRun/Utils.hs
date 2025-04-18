{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RugRun.Utils
  ( sha256
  , emitEvent
  , verifySecret
  ) where

import           PlutusTx.Prelude
import qualified Prelude                     as Haskell
import           Plutus.V1.Ledger.Api        (BuiltinByteString)
import qualified PlutusTx.Builtins           as Builtins

-- | Compute SHA-256 hash of input
sha256 :: BuiltinByteString -> BuiltinByteString
sha256 = Builtins.sha2_256

-- | Verify if a provided secret phrase matches the expected hash
verifySecret :: BuiltinByteString -> BuiltinByteString -> Bool
verifySecret secretHash providedSecret = sha256 providedSecret == secretHash

-- | Emit an on-chain event (this is a placeholder - actual implementation depends on the chain)
emitEvent :: BuiltinByteString -> BuiltinByteString -> Builtins.BuiltinData
emitEvent eventName eventData = Builtins.mkI 0  -- Placeholder implementation 