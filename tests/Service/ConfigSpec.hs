module Service.ConfigSpec(spec) where

import Test.Hspec
import Service.Config
import Data.Text
import Happstack.Server (Conf(..))
import Data.Ini.Config
import qualified Data.ByteString.Lazy as ByteString
import Data.Time.Clock (secondsToNominalDiffTime)
import Pages.Auth (AuthConf(..))
import Data.Either
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)


spec :: Spec
spec = do
  describe "parsePort" $ do
    it "should not accept non-number" $ do
      (parsePort $ pack "abc") `shouldBe` Left "Port number must be an integer"
    it "should not accept string ending with non-number" $ do
      (parsePort $ pack "8080abc") `shouldBe` Left "Port number must be an integer"
    it "should not accept empty string" $ do
      (parsePort $ pack "") `shouldBe` Left "Port number must be an integer"
    it "should not accept negative port number" $ do
      (parsePort $ pack "-15") `shouldBe` Left "Port number must be in range 1..65535"
    it "should not accept too large number" $ do
      (parsePort $ pack "8000000") `shouldBe` Left "Port number must be in range 1..65535"
    it "should accept port number" $ do
      (parsePort $ pack "8080") `shouldBe` Right 8080

  describe "parseInterval" $ do
    it "should not accept empty string" $ do
      (parseInterval $ pack "") `shouldBe` Left "Interval value must start with an integer"
    it "should not accept string starting with non-digits" $ do
      (parseInterval $ pack "abc") `shouldBe` Left "Interval value must start with an integer"
    it "should not accept string with unknown prefix" $ do
      (parseInterval $ pack "145f") `shouldBe` Left "Unknown unit: 'f'"
    it "should parse the number of seconds without suffix" $ do
      (parseInterval $ pack "145") `shouldBe` Right 145
    it "should parse the number of seconds" $ do
      (parseInterval $ pack "18s") `shouldBe` Right 18
    it "should parse the number of minutes" $ do
      (parseInterval $ pack "10m") `shouldBe` Right 600
    it "should parse the number of hours" $ do
      (parseInterval $ pack "2h") `shouldBe` Right 7200
    it "should parse the number of days" $ do
      (parseInterval $ pack "4d") `shouldBe` Right 345600

  describe "netConfigParser" $ do
    it "should return default config if no [net] section is present" $ do
      let Right conf = parseIniFile (pack "") netConfigParser
      (port conf, timeout conf) `shouldBe` (8080, 300)
    it "should return default config for missing properties" $ do
      let Right conf = parseIniFile (pack "[net]\na = 5\n") netConfigParser
      (port conf, timeout conf) `shouldBe` (8080, 300)
    it "should change port and timeout" $ do
      let Right conf = parseIniFile (pack "[net]\nport = 4000\ntimeout = 100\n") netConfigParser
      (port conf, timeout conf) `shouldBe` (4000, 100)
    it "should report error on invalid timeout" $ do
      let Left message = (parseIniFile (pack "[net]\ntimeout = abc\n") netConfigParser)
      message `shouldBe` "Line 2, in section \"net\": Interval value must start with an integer"
    it "should report error on invalid timeout" $ do
      let Left message = (parseIniFile (pack "[net]\nport = abc\n") netConfigParser)
      message `shouldBe` "Line 2, in section \"net\": Port number must be an integer"

  describe "authConfigParser" $ do
    it "should parse auth config" $ do
      let ini = "[auth]\nsecret = 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f\nsession-timeout = 100\npassword-hash = 505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f\n"
      let Right authConf = parseIniFile (pack ini) authConfigParser
      authConf `shouldBe` AuthConf { auConfTimeOut = secondsToNominalDiffTime 100
                                   , auConfSecret = ByteString.pack [0x00..0x1F]
                                   , auConfPasswordHash = ByteString.pack [0x50..0x6F] }
    it "should use default value for timeout" $ do
      let ini = "[auth]\nsecret = 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f\npassword-hash = 505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f\n"
      let Right authConf = parseIniFile (pack ini) authConfigParser
      authConf `shouldBe` AuthConf { auConfTimeOut = secondsToNominalDiffTime $ 60 * 60 * 24
                                   , auConfSecret = ByteString.pack [0x00..0x1F]
                                   , auConfPasswordHash = ByteString.pack [0x50..0x6F] }
    it "should fail if no secret is present" $ do
      let ini = "[auth]\nsession-timeout = 100\npassword-hash = 505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f\n"
      let Left message = parseIniFile (pack ini) authConfigParser
      message `shouldBe` "Missing field \"secret\" in section \"auth\""
    it "should fail if no password is present" $ do
      let ini = "[auth]\nsession-timeout = 100\nsecret = 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f\n"
      let Left message = parseIniFile (pack ini) authConfigParser
      message `shouldBe` "Missing field \"password-hash\" in section \"auth\""
    it "should fail if password has wrong length present" $ do
      let ini = "[auth]\nsecret = 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f\nsession-timeout = 100\npassword-hash = 505152535455565758595a5b5c5d5e5f\n"
      let Left message = parseIniFile (pack ini) authConfigParser
      message `shouldBe` "Line 4, in section \"auth\": The string must be 32 bytes"
    it "should fail if secret has wrong length present" $ do
      let ini = "[auth]\nsecret = 000102030405060708090a0b0c0d0e0f\nsession-timeout = 100\npassword-hash = 505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f\n"
      let Left message = parseIniFile (pack ini) authConfigParser
      message `shouldBe` "Line 2, in section \"auth\": The string must be 32 bytes"

  describe "loadConfig" $ do
    it "should parse config file" $ do
      let ini = "[docs]\npath = /path/to/directory/\n\
                \[auth]\nsecret = 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f\n\
                         \password-hash = 505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f\n\
                         \session-timeout = 200\n\
                \[net]\nport=4000\ntimeout=100\n"

      withSystemTempDirectory  "database" (\path -> do
        let filename = path ++ "/config.ini"
        writeFile filename ini
        Right (netConf, authConf, path) <- loadConfig filename
        path `shouldBe` "/path/to/directory/"
        authConf `shouldBe` AuthConf { auConfTimeOut = secondsToNominalDiffTime $ 200
                                     , auConfSecret = ByteString.pack [0x00..0x1F]
                                     , auConfPasswordHash = ByteString.pack [0x50..0x6F] }
        (port netConf, timeout netConf) `shouldBe` (4000, 100))
    it "shoud create a config file" $ do
      withSystemTempDirectory  "database" (\path -> do
        let filename = path ++ "/config.ini"
        res <- loadConfig filename
        isRight res `shouldBe` True
        fileExists <- doesFileExist filename
        fileExists `shouldBe` True)

