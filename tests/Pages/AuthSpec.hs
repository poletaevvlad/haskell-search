module Pages.AuthSpec (spec) where

import Prelude hiding (length)
import Test.Hspec
import Pages.Auth
import Data.ByteString.Lazy hiding (repeat, take)
import qualified Data.Binary as B
import qualified Data.Time.Clock as Clock
import Data.Time.Clock.POSIX
import qualified Crypto.Cipher.AES as AES
import Data.Binary(Binary)


key :: ByteString
key = pack [0x75, 0x77, 0x21, 0x7A, 0x25, 0x43, 0x2A, 0x46,
            0x2D, 0x4A, 0x61, 0x4E, 0x64, 0x52, 0x67, 0x55,
            0x6B, 0x58, 0x70, 0x32, 0x73, 0x35, 0x76, 0x38,
            0x79, 0x2F, 0x41, 0x3F, 0x44, 0x28, 0x47, 0x2B]

iv :: ByteString
iv = pack [16..31]


pad :: ByteString -> ByteString
pad string =
  mconcat [string, pack $ take (fromIntegral $ 16 - (length string `mod` 16)) $ repeat (0 :: B.Word8)]


encrypt :: (Binary b) => b -> ByteString
encrypt v = let aes = AES.initAES $ toStrict key
            in fromStrict $ AES.encryptCBC aes (toStrict iv) $ toStrict $ pad $ B.encode v

testBinary :: (B.Binary m, Show m, Eq m) => m -> ByteString -> SpecWith ()
testBinary obj bytes = do
  it "should serialize" $ do
    B.encode obj `shouldBe` bytes
  it "should deserialzie" $ do
    B.decode bytes `shouldBe` obj


addTime :: Clock.UTCTime -> Int -> Clock.UTCTime
addTime time diff =
  let timeDiff = Clock.secondsToNominalDiffTime $ fromIntegral diff
  in Clock.addUTCTime timeDiff time


spec :: Spec
spec = do
  let tokenString = pack [1..32]
  testBinary (Token tokenString) tokenString

  describe "Binary TokenStruct" $ do
    let tokenStructString = mconcat [pack [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0], tokenString]
    let time = posixSecondsToUTCTime $ fromIntegral 0x123456789abcdef0
    let tokenStruct = TokenStruct time $ Token tokenString
    testBinary tokenStruct tokenStructString

  describe "Binary AuthSecret" $ do
    let bs = pack [0x01, 0x02, 0x03, 0x04]
    let encoded = mconcat [tokenString, B.encode (4 :: B.Word64), bs]
    let decoded = AuthSecret (Token tokenString) bs
    testBinary decoded encoded

  let conf = AuthConf { auConfTimeOut = 5000, auConfSecret = key, auConfPasswordHash = pack [] }
  describe "validateAuthSecret" $ do
    it "should not validate if string is not a valid secret object" $ do
      time <- Clock.getCurrentTime
      validateAuthSecret conf time (pack [0x12, 0x13, 0x14]) `shouldBe` False
    it "should not validate if IV is too short" $ do
      time <- Clock.getCurrentTime
      let message = mconcat [tokenString, B.encode (3 :: B.Word64), pack [0x12, 0x13, 0x14]]
      validateAuthSecret conf time message `shouldBe` False
    it "should not validate if encrypted message is too short" $ do
      time <- Clock.getCurrentTime
      let message = mconcat [tokenString, B.encode (19 :: B.Word64), iv, pack [0x12, 0x13, 0x14]]
      validateAuthSecret conf time message `shouldBe` False
    it "should not validate if encrypted message is not a valid string" $ do
      time <- Clock.getCurrentTime
      let message = mconcat [tokenString, B.encode (19 :: B.Word64), iv, pack [0x12, 0x13, 0x14]]
      validateAuthSecret conf time message `shouldBe` False
    it "should not validate if tokens do not match" $ do
      time <- Clock.getCurrentTime
      let encrypted = encrypt $ TokenStruct (time `addTime` 1000) $ Token $ pack [2..33]
      let message = mconcat [tokenString, B.encode (fromIntegral $ length encrypted :: B.Word64), iv, encrypted]
      validateAuthSecret conf time message `shouldBe` False
    it "should not validate if secret is expired" $ do
      time <- Clock.getCurrentTime
      let encrypted = encrypt $ TokenStruct (time `addTime` 8000) $ Token tokenString
      let message = mconcat [tokenString, B.encode (fromIntegral $ length encrypted :: B.Word64), iv, encrypted]
      validateAuthSecret conf time message `shouldBe` False
    it "should not validate secrets" $ do
      time <- Clock.getCurrentTime
      let encrypted = encrypt $ TokenStruct (time `addTime` 4000) $ Token tokenString
      let message = mconcat [tokenString, B.encode (fromIntegral $ length encrypted :: B.Word64), iv, encrypted]
      validateAuthSecret conf time message `shouldBe` False
  describe "generateAuthSecret" $ do
    it "should generate dirrerent secrets" $ do
      secret1 <- generateAuthSecret conf
      secret2 <- generateAuthSecret conf
      secret1 `shouldNotBe` secret2
    it "should generate valid secret" $ do
      secret <- generateAuthSecret conf
      time <- Clock.getCurrentTime
      validateAuthSecret conf time secret `shouldBe` True
    it "should generate expiring secret" $ do
      secret <- generateAuthSecret conf
      time <- Clock.getCurrentTime
      validateAuthSecret conf (time `addTime` 6000) secret `shouldBe` False

  describe "checkPassword" $ do
    let hash = [0x5e, 0x88, 0x48, 0x98, 0xda, 0x28, 0x04, 0x71,
                0x51, 0xd0, 0xe5, 0x6f, 0x8d, 0xc6, 0x29, 0x27,
                0x73, 0x60, 0x3d, 0x0d, 0x6a, 0xab, 0xbd, 0xd6,
                0x2a, 0x11, 0xef, 0x72, 0x1d, 0x15, 0x42, 0xd8] :: [B.Word8]
    it "validates password if passwords match" $ do
      let config = conf { auConfPasswordHash = pack hash }
      checkPassword config "password" `shouldBe` True
    it "does not validate password if passwords do not match" $ do
      let config = conf { auConfPasswordHash = pack hash }
      checkPassword config "hunter2" `shouldBe` False
