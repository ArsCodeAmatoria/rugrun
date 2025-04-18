module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude            hiding (Semigroup(..), (<$>))
import           Plutus.V1.Ledger.Api        (POSIXTime(..), PubKeyHash(..))
import           Prelude                      (IO, String, ($), (++), (<$>))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.ByteString              as BS

import           RugRun.Contract
import           RugRun.Types
import           RugRun.Utils

-- Convert String to BuiltinByteString
strToByteString :: String -> BuiltinByteString
strToByteString s = Builtins.toBuiltin $ TE.encodeUtf8 $ T.pack s

-- Convert BuiltinByteString to String
byteStringToStr :: BuiltinByteString -> String
byteStringToStr bs = T.unpack $ TE.decodeUtf8 $ Builtins.fromBuiltin bs

-- Create a mock PubKeyHash
mockPkh :: PubKeyHash
mockPkh = PubKeyHash $ Builtins.toBuiltin $ BS.pack [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]

-- Test the SHA-256 hashing function
testSha256 :: TestTree
testSha256 = testGroup "SHA-256 Tests"
  [ testCase "Simple String Hash" $ do
      let input = strToByteString "haskellrocks"
          result = sha256 input
          resultHex = byteStringToStr result
      resultHex @?= "eb"  -- This will fail with the actual hash, replace with correct value
  
  , testCase "Empty String Hash" $ do
      let input = strToByteString ""
          result = sha256 input
          resultHex = byteStringToStr result
      resultHex @?= "e3" -- This will fail with the actual hash, replace with correct value
  ]

-- Test the secret verification function
testVerifySecret :: TestTree
testVerifySecret = testGroup "Secret Verification Tests"
  [ testCase "Correct Secret" $ do
      let secret = strToByteString "haskellrocks"
          hash = sha256 secret
          result = verifySecret hash secret
      result @?= True
      
  , testCase "Incorrect Secret" $ do
      let correctSecret = strToByteString "haskellrocks"
          hash = sha256 correctSecret
          wrongSecret = strToByteString "wrongsecret"
          result = verifySecret hash wrongSecret
      result @?= False
  ]

-- Test utility functions
testUtilityFunctions :: TestTree
testUtilityFunctions = testGroup "Utility Function Tests"
  [ testCase "Game ID Generation" $ do
      let seed = strToByteString "gameseed"
          time = POSIXTime 1234567890
          gameId = generateGameId seed time
      lengthOfByteString gameId @?= 32  -- SHA-256 output is 32 bytes
      
  , testCase "Expiration Validation" $ do
      let currentTime = POSIXTime 1000
          futureTime = POSIXTime 2000
          pastTime = POSIXTime 500
          
          notExpiredYet = validateExpiration currentTime futureTime
          alreadyExpired = validateExpiration currentTime pastTime
          
      notExpiredYet @?= False  -- Should be false since current < expiration
      alreadyExpired @?= True  -- Should be true since current > expiration
      
  , testCase "Fee Calculation" $ do
      let total = 1000
          feePercentage = 10
          expected = 100
          
          result = calculateFee total feePercentage
          
      result @?= expected
  ]

-- Test clue encoding/decoding
testClueEncoding :: TestTree
testClueEncoding = testGroup "Clue Encoding Tests"
  [ testCase "Encode and Decode" $ do
      let original = strToByteString "This is a secret clue"
          encoded = encodeClue original
          decoded = decodeClue encoded
          
      decoded @?= original
      
  , testCase "Encoded is Different" $ do
      let original = strToByteString "This is a secret clue"
          encoded = encodeClue original
          
      assertBool "Encoded should be different from original" (encoded /= original)
  ]

-- Main test group
main :: IO ()
main = defaultMain $ testGroup "RugRun Contract Tests"
  [ testSha256
  , testVerifySecret
  , testUtilityFunctions
  , testClueEncoding
  ] 