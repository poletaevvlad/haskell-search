module DocumentsSpec(spec) where

import Test.Hspec
import Documents
    

spec :: Spec
spec = do
  describe "formatFileSize" $ do
    it "must format empty file" $ do
      formatFileSize 0 `shouldBe` "empty"
    it "must format one-byte-lonf files" $ do
      formatFileSize 1 `shouldBe` "1 byte"
    
    it "must format files less then a KiB" $ do
      formatFileSize 500 `shouldBe` "500 bytes"
    it "must format files less then 10 KiB" $ do
      formatFileSize 1200 `shouldBe` "1.2 KiB"
      
    it "must format files less then 1 MiB" $ do
      formatFileSize 24576 `shouldBe` "24 KiB"
    it "must format files less then 10 MiB" $ do
      formatFileSize 4404029 `shouldBe` "4.2 MiB"
    
    it "must format files more then 10 MiB" $ do
      formatFileSize 498073600 `shouldBe` "475 MiB"
  
  describe "escapeFileName" $ do
    it "returns value as is if valid" $ do
      escapeFileName "validfilename" `shouldBe` "validfilename"
    it "substitutes spaces" $ do
      escapeFileName "valid file name" `shouldBe` "valid-file-name"
    it "converts to lowercase" $ do
      escapeFileName "ValidName" `shouldBe` "validname"
    it "ignores double spaces" $ do
      escapeFileName "valid   name" `shouldBe` "valid-name"
    it "ignores punctuation" $ do
      escapeFileName "valid, - name" `shouldBe` "valid-name"
    it "ignores apostrophies" $ do
      escapeFileName "don't haven't" `shouldBe` "dont-havent"
    it "supports non latin scripts" $ do
      escapeFileName "Правильное название" `shouldBe` "правильное-название"
