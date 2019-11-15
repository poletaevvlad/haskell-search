module Pages.Auth (AuthConf(..), Token(Token), TokenStruct(TokenStruct),
  AuthSecret(AuthSecret), validateAuthSecret, generateAuthSecret,
  checkPassword) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Lazy as ByteString
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime,
  secondsToNominalDiffTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Crypto.Cipher.AES (decryptCBC, initAES, encryptCBC)
import Data.Maybe (isJust)
import Crypto.Random (newGenIO, genBytes, CryptoRandomGen)
import Crypto.Random.DRBG (HmacDRBG)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy.UTF8 as BLU

data AuthConf =
  AuthConf { auConfTimeOut :: NominalDiffTime
           , auConfSecret :: ByteString
           , auConfPasswordHash :: ByteString }


data Token = Token ByteString deriving (Show, Eq)

tokenSize :: Int
tokenSize = 32

instance Binary Token where
  put (Token string) = foldl1 (>>) $ map Binary.put $ ByteString.unpack string
  get = Token <$> ByteString.pack <$> readString tokenSize
    where readString :: Int -> Binary.Get [Binary.Word8]
          readString 0 = return []
          readString n = (:) <$> Binary.get <*> readString (n - 1)

instance Binary UTCTime where
  put time = Binary.put $ (floor $ nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time :: Binary.Word64)
  get = (posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral) <$> (Binary.get :: Binary.Get Binary.Word64)

data TokenStruct = TokenStruct UTCTime Token deriving (Show, Eq)

instance Binary TokenStruct where
  put (TokenStruct time token) = Binary.put time >> Binary.put token
  get = (TokenStruct) <$> Binary.get <*> Binary.get


data AuthSecret = AuthSecret Token ByteString deriving (Show, Eq)

instance Binary AuthSecret where
  put (AuthSecret token encrypted) = Binary.put token >> Binary.put encrypted
  get = (AuthSecret) <$> Binary.get <*> Binary.get


-- Validating auth secret

maybeDecode :: (Binary m) => ByteString -> Maybe m
maybeDecode string =
  case Binary.decodeOrFail string of
    Left _ -> Nothing
    Right (_, _, x) -> Just x

maybeDecrypt :: AuthConf -> ByteString -> Maybe ByteString
maybeDecrypt conf message = do
  let len = ByteString.length message
  validIf $ len > 16
  validIf $ len `mod` 16 == 0

  let aes = initAES $ ByteString.toStrict $ auConfSecret conf
  let strict = ByteString.toStrict message
  let iv = BSStrict.take 16 strict
  let ciphertext = BSStrict.drop 16 strict
  return $ ByteString.fromStrict $ decryptCBC aes iv ciphertext

validIf :: Bool -> Maybe ()
validIf True = Just ()
validIf False = Nothing


validateAuthSecret :: AuthConf -> UTCTime -> ByteString -> Bool
validateAuthSecret conf currentTime secretString = isJust $ do
  (AuthSecret token encrypted) <- maybeDecode secretString
  (TokenStruct time token2) <- maybeDecrypt conf encrypted >>= maybeDecode
  validIf $ token == token2
  validIf $ time <= currentTime
  validIf $ currentTime < (addUTCTime (auConfTimeOut conf) time)


encrypt :: AuthConf -> ByteString -> ByteString -> ByteString
encrypt conf iv message = ByteString.fromStrict $ encryptCBC aes (ByteString.toStrict iv) padded
  where
    aes = initAES $ ByteString.toStrict $ auConfSecret conf
    len = ByteString.length message
    padded = ByteString.toStrict $ if len `mod` 16 == 0 then message
      else mconcat [message, ByteString.pack $ take (fromIntegral $ len `mod` 16) $ repeat 0]


generateRandom :: (CryptoRandomGen g) => Int -> g -> (ByteString, g)
generateRandom size gen =
  case genBytes size gen of
    Left err -> error $ show err
    Right (string, newGen) -> (ByteString.fromStrict string, newGen)


generateAuthSecret :: AuthConf -> IO ByteString
generateAuthSecret conf = do
  time <- getCurrentTime
  (tokenString, gen1) <- generateRandom tokenSize <$> (newGenIO :: IO HmacDRBG)
  let (iv, _) = generateRandom 16 gen1
  let token = Token tokenString
  let encrypted = mconcat [iv, encrypt conf iv $ Binary.encode $ TokenStruct time token]
  return $ Binary.encode $ AuthSecret token encrypted


-- password verification

checkPassword :: AuthConf -> String -> Bool
checkPassword conf password =
  let hash = ByteString.fromStrict $ SHA256.hashlazy $ BLU.fromString password
  in hash == auConfPasswordHash conf
