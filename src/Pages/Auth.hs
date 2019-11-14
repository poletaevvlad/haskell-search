module Pages.Auth (Token(Token), TokenStruct(TokenStruct),
  AuthSecret(AuthSecret)) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Binary (Binary)
import qualified Data.Binary as Binary


data AuthConf =
  AuthConf { auConfTimeOut :: Int
           , auConfSecret :: ByteString
           }


data Token = Token ByteString deriving (Show, Eq)

tokenSize :: Int
tokenSize = 32

instance Binary Token where
  put (Token string) = foldl1 (>>) $ map Binary.put $ ByteString.unpack string
  get = Token <$> ByteString.pack <$> readString tokenSize
    where readString :: Int -> Binary.Get [Binary.Word8]
          readString 0 = return []
          readString n = (:) <$> Binary.get <*> readString (n - 1)


data TokenStruct = TokenStruct Binary.Word64 Token deriving (Show, Eq)

instance Binary TokenStruct where
  put (TokenStruct time token) = Binary.put time >> Binary.put token
  get = (TokenStruct) <$> Binary.get <*> Binary.get


data AuthSecret = AuthSecret Token ByteString deriving (Show, Eq)

instance Binary AuthSecret where
  put (AuthSecret token encrypted) = Binary.put token >> Binary.put encrypted
  get = (AuthSecret) <$> Binary.get <*> Binary.get
