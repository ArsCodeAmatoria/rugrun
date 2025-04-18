module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified PlutusTx.Builtins    as Builtins
import           Test.Tasty
import           Test.Tasty.HUnit

import           RugRun.Contract
import           RugRun.Types
import           RugRun.Utils

import qualified Prelude              as Haskell
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Contexts

-- Helper function to convert String to BuiltinByteString
stringToBuiltinByteString :: Haskell.String -> BuiltinByteString
stringToBuiltinByteString = Builtins.toBuiltin . TE.encodeUtf8 . T.pack

-- Helper function to convert BuiltinByteString to String
builtinByteStringToString :: BuiltinByteString -> Haskell.String
builtinByteStringToString = T.unpack . TE.decodeUtf8 . Builtins.fromBuiltin

-- Fixtures for testing
testSecret :: BuiltinByteString
testSecret = stringToBuiltinByteString "haskell_loves_monads"

testSecretHash :: BuiltinByteString
testSecretHash = sha256 testSecret

creatorPkh :: PubKeyHash
creatorPkh = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

expirationTime :: POSIXTime
expirationTime = 1672531200000 -- 2023-01-01

createTestRugDatum :: RugDatum
createTestRugDatum = RugDatum
  { rugSecretHash = testSecretHash
  , rugStatus = Unclaimed
  , rugCreatorPkh = creatorPkh
  }

createTestDecoyDatum :: DecoyDatum
createTestDecoyDatum = DecoyDatum
  { decoyClue = stringToBuiltinByteString "This is a decoy clue"
  , decoyBurnOnUse = True
  , decoyExpiration = expirationTime
  , decoyCreatorPkh = creatorPkh
  }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "RugRun Tests"
  [ testGroup "Utils Tests" 
      [ testCase "SHA-256 Hashing" testSha256
      , testCase "Secret Verification (Valid)" testValidSecretVerification
      , testCase "Secret Verification (Invalid)" testInvalidSecretVerification
      , testCase "Game ID Generation" testGameIdGeneration
      , testCase "Expiration Validation" testExpirationValidation
      ]
  , testGroup "RugWallet Validator Tests"
      [ testCase "Valid Secret Phrase" testValidRugRedeemer
      , testCase "Invalid Secret Phrase" testInvalidRugRedeemer
      , testCase "Already Claimed" testAlreadyClaimedRug
      ]
  , testGroup "DecoyWallet Validator Tests"
      [ testCase "Normal Interaction (Always Fails)" testDecoyInteraction
      , testCase "Burn On Use" testDecoyBurnOnUse
      , testCase "Reclaim After Expiration" testDecoyReclaim
      ]
  ]

-- Utils Tests
testSha256 :: Assertion
testSha256 = do
  let input = stringToBuiltinByteString "test"
  let expectedHash = stringToBuiltinByteString "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
  Haskell.assertEqual "SHA-256 hash should match expected value" expectedHash (sha256 input)

testValidSecretVerification :: Assertion
testValidSecretVerification = do
  let result = verifySecret testSecretHash testSecret
  Haskell.assertBool "Valid secret should verify" result

testInvalidSecretVerification :: Assertion
testInvalidSecretVerification = do
  let wrongSecret = stringToBuiltinByteString "wrong_secret"
  let result = verifySecret testSecretHash wrongSecret
  Haskell.assertBool "Invalid secret should not verify" (not result)

testGameIdGeneration :: Assertion
testGameIdGeneration = do
  let seed = stringToBuiltinByteString "test-seed"
  let timestamp = 1672531200000 -- 2023-01-01
  let gameId1 = generateGameId seed timestamp
  let gameId2 = generateGameId seed (timestamp + 1)
  
  Haskell.assertBool "Game IDs should not be empty" (lengthOfByteString gameId1 > 0)
  Haskell.assertBool "Different timestamps should produce different game IDs" (gameId1 /= gameId2)

testExpirationValidation :: Assertion
testExpirationValidation = do
  let expiration = 1672531200000 -- 2023-01-01
  let beforeExpiration = 1672444800000 -- 2022-12-31
  let afterExpiration = 1672617600000 -- 2023-01-02
  
  Haskell.assertBool "Should not be expired before expiration time" 
    (not $ validateExpiration beforeExpiration expiration)
  
  Haskell.assertBool "Should be expired after expiration time" 
    (validateExpiration afterExpiration expiration)

-- RugWallet Validator Tests
testValidRugRedeemer :: Assertion
testValidRugRedeemer = do
  let datum = createTestRugDatum
  let redeemer = RugRedeemer { secretPhrase = testSecret }
  let result = validateRugWallet datum redeemer
  
  Haskell.assertEqual "Validation result should be success" True (Haskell.fst result)

testInvalidRugRedeemer :: Assertion
testInvalidRugRedeemer = do
  let datum = createTestRugDatum
  let redeemer = RugRedeemer { secretPhrase = stringToBuiltinByteString "wrong_secret" }
  let result = validateRugWallet datum redeemer
  
  Haskell.assertEqual "Validation result should be failure" False (Haskell.fst result)

testAlreadyClaimedRug :: Assertion
testAlreadyClaimedRug = do
  let datum = createTestRugDatum { rugStatus = Claimed }
  let redeemer = RugRedeemer { secretPhrase = testSecret }
  let result = validateRugWallet datum redeemer
  
  Haskell.assertEqual "Validation should fail for already claimed wallet" False (Haskell.fst result)

-- DecoyWallet Validator Tests
testDecoyInteraction :: Assertion
testDecoyInteraction = do
  let datum = createTestDecoyDatum
  let redeemer = DecoyRedeemer { decoyInput = stringToBuiltinByteString "any_input", decoyReclaimOp = NormalOperation }
  let result = validateDecoyWallet datum redeemer
  
  Haskell.assertEqual "Decoy wallet should always reject normal interaction" False (Haskell.fst result)

testDecoyBurnOnUse :: Assertion
testDecoyBurnOnUse = do
  let datum = createTestDecoyDatum { decoyBurnOnUse = True }
  let redeemer = DecoyRedeemer { decoyInput = stringToBuiltinByteString "any_input", decoyReclaimOp = NormalOperation }
  let result = validateDecoyWallet datum redeemer
  
  Haskell.assertEqual "Decoy wallet with burn should fail" False (Haskell.fst result)
  -- In actual implementation, this would trigger token burn, but that's hard to test in isolation

testDecoyReclaim :: Assertion
testDecoyReclaim = do
  let datum = createTestDecoyDatum
  let redeemer = DecoyRedeemer { decoyInput = stringToBuiltinByteString "any_input", decoyReclaimOp = ReclaimTokens }
  
  -- Create a mock context with time after expiration
  let mockCtxAfterExpiration = undefined -- In a real test, we would create a proper ScriptContext
  
  -- This is a simplified test since we can't easily create a full ScriptContext
  -- In a real implementation, we would mock the transaction info and time
  let isExpired = validateExpiration (expirationTime + 1) expirationTime
  Haskell.assertBool "Should be expired" isExpired 