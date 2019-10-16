module Search.PorterSpec(spec) where

import Test.Hspec
import Search.Porter
import Control.Monad


generateTests :: String -> [(String, String)] -> Spec
generateTests stepName examples = 
  describe stepName $ do
    forM_ examples generate
  where 
    generate :: (String, String) -> Spec
    generate (request, responce) = do
      it request $ do
        porter request `shouldBe` responce


spec :: Spec
spec = do
  describe "porter" $ do
    describe "Step 1" $ do
      generateTests "Step 1a" [
        ("caresses", "caress"),
        ("ponies", "poni"),
        ("ties", "ti"),
        ("caress", "caress"),
        ("cats", "cat")]
      generateTests "Step 1b" [
        ("feed", "feed"),
        ("agreed", "agre"),
        ("plastered", "plaster"),
        ("bled", "bled"),
        ("motoring", "motor"),
        ("sing", "sing"),
        ("conflated", "conflat"),
        ("troubled", "troubl"),
        ("sized", "size"),
        ("hopping", "hop"),
        ("tanned", "tan"),
        ("falling", "fall"),
        ("hissing", "hiss"),
        ("fizzed", "fizz"),
        ("failing", "fail"),
        ("filing", "file")]
      generateTests "Step 1c" [
        ("happy", "happi"),
        ("sky", "sky")]
    generateTests "Step 2" [
      ("relational", "relat"),
      ("conditional", "condit"),
      ("rational", "ration"),
      ("valenci", "valenc"),
      ("hesitanci", "hesit"),
      ("digitizer", "digit"),
      ("conformabli", "conform"),
      ("radicalli", "radic"),
      ("differentli", "differ"),
      ("vileli", "vile"),
      ("analogousli", "analog"),
      ("vietnamization", "vietnam"),
      ("predication", "predic"),
      ("operator", "oper"),
      ("feudalism", "feudal"),
      ("decisiveness", "decis"),
      ("hopefulness", "hope"),
      ("callousness", "callous"),
      ("formaliti", "formal"),
      ("sensitiviti", "sensit"),
      ("sensibiliti", "sensibl")]
    generateTests "Step 3" [
      ("triplicate", "triplic"),
      ("formative", "form"),
      ("formalize", "formal"),
      ("electriciti", "electr"),
      ("electrical", "electr"),
      ("hopeful", "hope"),
      ("goodness", "good")] 
    generateTests "Step 4" [
      ("revival", "reviv"),
      ("allowance", "allow"),
      ("inference", "infer"),
      ("airliner", "airlin"),
      ("gyroscopic", "gyroscop"),
      ("adjustable", "adjust"),
      ("defensible", "defens"),
      ("irritant", "irrit"),
      ("replacement", "replac"),
      ("adjustment", "adjust"),
      ("dependent", "depend"),
      ("adoption", "adopt"),
      ("homologou", "homolog"),
      ("communism", "commun"),
      ("activate", "activ"),
      ("angulariti", "angular"),
      ("homologous", "homolog"),
      ("effective", "effect"),
      ("bowdlerize", "bowdler")] 
    describe "Step 5" $ do
      generateTests "Step 5a" [
        ("probate", "probat"),
        ("rate", "rate"),
        ("cease", "ceas")]
      generateTests "Step 5a" [
        ("controll", "control"),
        ("roll", "roll")]
