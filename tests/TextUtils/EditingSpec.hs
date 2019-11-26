module TextUtils.EditingSpec(spec) where

import Test.Hspec
import TextUtils.Editing


spec :: Spec
spec = do
  let text = [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
             , "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
             , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur." ]
  let resText = "Lorem ipsum dolor sit amet, consectetur\n\
      \adipiscing elit, sed do eiusmod tempor\n\
      \incididunt ut labore et dolore magna\n\
      \aliqua.\n\
      \\n\
      \Ut enim ad minim veniam, quis nostrud\n\
      \exercitation ullamco laboris nisi ut\n\
      \aliquip ex ea commodo consequat.\n\
      \\n\
      \Duis aute irure dolor in reprehenderit\n\
      \in voluptate velit esse cillum dolore eu\n\
      \fugiat nulla pariatur."

  describe "toEditor" $ do
    it "should format strings list" $ do
      toEditor text 40 `shouldBe` resText
    it "should not add newline before long word" $ do
      toEditor ["1234567890123 456 789"] 10 `shouldBe` "1234567890123\n456 789"



