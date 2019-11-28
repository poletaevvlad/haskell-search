module Service.ConfigSpec(spec) where

import Test.Hspec
import Service.Config
import Data.Text
import Happstack.Server (Conf(..))
import Data.Ini.Config


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

