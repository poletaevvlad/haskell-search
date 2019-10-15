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
    

    