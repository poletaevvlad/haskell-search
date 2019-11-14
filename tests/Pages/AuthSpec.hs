module Pages.AuthSpec (spec) where

import Test.Hspec
import Pages.Auth
import Data.ByteString.Lazy
import qualified Data.Binary as B


testBinary :: (B.Binary m, Show m, Eq m) => m -> ByteString -> SpecWith ()
testBinary obj bytes = do
  it "should serialize" $ do
    B.encode obj `shouldBe` bytes
  it "should deserialzie" $ do
    B.decode bytes `shouldBe` obj


spec :: Spec
spec = do
  let tokenString = pack [1..32]
  testBinary (Token tokenString) tokenString

  describe "Binary TokenStruct" $ do
    let tokenStructString = mconcat [pack [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0], tokenString]
    let tokenStruct = TokenStruct 0x123456789abcdef0 $ Token tokenString
    testBinary tokenStruct tokenStructString

  describe "Binary AuthSecret" $ do
    let bs = pack [0x01, 0x02, 0x03, 0x04]
    let encoded = mconcat [tokenString, B.encode (4 :: B.Word64), bs]
    let decoded = AuthSecret (Token tokenString) bs
    testBinary decoded encoded
