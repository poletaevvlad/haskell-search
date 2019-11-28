{-# LANGUAGE OverloadedStrings #-}
module Service.Config (parsePort, parseInterval, netConfigParser) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import Data.Char (isNumber)
import Data.List (span)
import Data.Ini.Config
import Happstack.Server(Conf(port, timeout), nullConf)


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

