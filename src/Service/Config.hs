{-# LANGUAGE OverloadedStrings #-}
module Service.Config (parsePort, parseInterval, netConfigParser,
  authConfigParser, loadConfig) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Char (isNumber)
import Data.List (span)
import Data.Ini.Config
import Happstack.Server(Conf(port, timeout), nullConf)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import TextUtils.Processing (hexDecode, hexEncode)
import Pages.Auth (AuthConf(..), generateRandom)
import Data.Time.Clock (secondsToNominalDiffTime)
import Crypto.Random (newGenIO)
import Crypto.Random.DRBG (HmacDRBG)
import System.Directory (getXdgDirectory, XdgDirectory(XdgData), doesFileExist)



parsePort :: Text -> Either String Int
parsePort text = case readMaybe $ Text.unpack text of
  Nothing -> Left "Port number must be an integer"
  Just port ->
    if port < 0 || port > 0xFFFF
      then Left "Port number must be in range 1..65535"
      else Right port


errorIf :: Bool -> String -> Either String ()
errorIf True message = Left message
errorIf False _ = Right ()


parseInterval :: Text -> Either String Int
parseInterval text = do
  let (number, suffix) = span isNumber $ Text.unpack text
  errorIf (null number) "Interval value must start with an integer"
  mul <- parseSuffix suffix
  return $ read number * mul
  where
    parseSuffix :: String -> Either String Int
    parseSuffix "" = Right 1
    parseSuffix "s" = Right 1
    parseSuffix "m" = Right 60
    parseSuffix "h" = Right $ 60 * 60
    parseSuffix "d" = Right $ 60 * 60 * 24
    parseSuffix unit = Left $ "Unknown unit: '" ++ unit ++ "'"


parseHexadecimal :: Int -> Text -> Either String ByteString
parseHexadecimal length text = case hexDecode $ Text.unpack text of
  Nothing -> Left "Not a hexadecimal string"
  Just byteString -> do
    let len = fromIntegral $ ByteString.length byteString :: Int
    errorIf (len /= length) ("The string must be " ++ show length ++ " bytes")
    return byteString


netConfigParser :: IniParser Conf
netConfigParser =
  sectionDef "net" defaultConf $ do
    port <- fieldDefOf "port" parsePort defaultPort
    timeout <- fieldDefOf "timeout" parseInterval defaultTimeout
    return nullConf { port = port, timeout = timeout }
  where
    defaultConf = nullConf { port = defaultPort, timeout = defaultTimeout }
    defaultPort = 8080
    defaultTimeout = 300


authConfigParser :: IniParser AuthConf
authConfigParser =
  section "auth" $ do
    timeout <- fieldDefOf "session-timeout" parseInterval (60 * 60 * 24)
    secret <- fieldOf "secret" $ parseHexadecimal 32
    password <- fieldOf "password-hash" $ parseHexadecimal 32
    return AuthConf { auConfTimeOut = secondsToNominalDiffTime $ fromIntegral timeout
                    , auConfSecret = secret
                    , auConfPasswordHash = password }


configParser :: IniParser (Conf, AuthConf, FilePath)
configParser = do
  netConf <- netConfigParser
  authConf <- authConfigParser
  path <- Text.unpack <$> (section "docs" $ field "path")
  return (netConf, authConf, path)


generageInitialConfig :: IO String
generageInitialConfig = do
  dataDir <- getXdgDirectory XdgData "webse"
  secret <- fst <$> generateRandom 32 <$> (newGenIO :: IO HmacDRBG)
  return $ mconcat [ section "docs"
                   , keyValue "path" $ dataDir
                   , section "auth"
                   , keyValue "secret" $ hexEncode secret
                   , keyValue "password-hash" $ hexEncode $ ByteString.pack $ take 32 $ repeat 0 ]

  where
    section :: String -> String
    section name = '[':name ++ "]\n"
    keyValue :: String -> String -> String
    keyValue key value = key ++ " = " ++ value ++ "\n"


loadConfig :: FilePath -> IO (Either String (Conf, AuthConf, FilePath))
loadConfig path = do
  fileExist <- doesFileExist path
  fileContents <- if fileExist
    then readFile path
    else do
      config <- generageInitialConfig
      writeFile path config
      return config
  return $ parseIniFile (Text.pack fileContents) configParser

