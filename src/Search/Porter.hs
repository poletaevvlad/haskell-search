module Search.Porter (porter) where
import Data.List(isSuffixOf, group)


data CharType = Vowel | Consonant
  deriving (Show, Eq)


assignConsonants :: String -> [CharType]
assignConsonants "" = []
assignConsonants (w:word) =
  scanl
  (\prevType currChar -> boolToType $ isVowel prevType currChar)
  (boolToType $ elem w simpleVowels)
  word
  where
  isVowel x 'y' = x == Consonant
  isVowel _ curr = curr `elem` simpleVowels

  boolToType True = Vowel
  boolToType False = Consonant
  simpleVowels = "aeiou"


getM :: [CharType] -> Int
getM text = computeM $ map head $ group text
  where
    computeM (Consonant:types) = computeM types
    computeM (Vowel:Consonant:types) = computeM types + 1
    computeM _ = 0


data Rule = Rule (String -> Bool) String String


noPredicate :: String -> Bool
noPredicate _ = True


requireM :: (Int -> Bool) -> String -> Bool
requireM predicate word = predicate $ getM $ assignConsonants word


applyRule :: Rule -> String -> (Bool, String)
applyRule (Rule predicate suffix repl) string
  | not (suffix `isSuffixOf` string) || suffix == string = (False, string)
  | not $ predicate withoutPrefix = (True, string)
  | otherwise = (True, withoutPrefix ++ repl)
  where withoutPrefix = take (length string - length suffix) string


applyStep :: [Rule] -> String -> String
applyStep [] string = string
applyStep (rule:rules) string
  | ruleMatched = result
  | otherwise = applyStep rules string
  where (ruleMatched, result) = applyRule rule string


-- Step 1

porterStep1a :: [Rule]
porterStep1a = [
  Rule noPredicate "sses" "ss",
  Rule noPredicate "ies" "i",
  Rule noPredicate "ss" "ss",
  Rule noPredicate "s" ""]


step1a :: String -> String
step1a = applyStep porterStep1a


step1bAdd :: String -> String
step1bAdd word
  | any (`isSuffixOf` word) ["at", "bl", "iz"] = word ++ "e"
  | otherwise = let
      revWord = reverse word
      types = assignConsonants revWord
    in if head revWord `notElem` "lsz" &&
        head types == Consonant &&
        head revWord == head (tail revWord)
      then init word
      else if take 3 types == [Consonant, Vowel, Consonant] &&
          head revWord `notElem` "wxy" &&
          getM types == 1
        then word ++ "e"
        else word


step1b :: String -> String
step1b word
  | "eed" `isSuffixOf` word =
      if getM (take (wordLen - 3) types) > 0
      then init word else word
  | "ed" `isSuffixOf` word =
      if Vowel `elem` take (wordLen - 2) types
      then step1bAdd $ take (length word - 2) word else word
  | "ing" `isSuffixOf` word =
      if Vowel `elem` take (wordLen - 3) types
      then step1bAdd $ take (length word - 3) word else word
  | otherwise = word
  where
    wordLen = length word
    types = assignConsonants word


step1c :: String -> String
step1c word
  | last word == 'y' && Vowel `elem` assignConsonants (init word) = init word ++ "i"
  | otherwise = word


-- Step 2

porterStep2 :: [Rule]
porterStep2 = [
  Rule (requireM (>0)) "ational" "ate",
  Rule (requireM (>0)) "tional" "tion",
  Rule (requireM (>0)) "enci" "ence",
  Rule (requireM (>0)) "anci" "ance",
  Rule (requireM (>0)) "izer" "ize",
  Rule (requireM (>0)) "abli" "able",
  Rule (requireM (>0)) "alli" "al",
  Rule (requireM (>0)) "entli" "ent",
  Rule (requireM (>0)) "eli" "e",
  Rule (requireM (>0)) "ousli" "ous",
  Rule (requireM (>0)) "ization" "ize",
  Rule (requireM (>0)) "ation" "ate",
  Rule (requireM (>0)) "ator" "ate",
  Rule (requireM (>0)) "alism" "al",
  Rule (requireM (>0)) "aliti" "al",
  Rule (requireM (>0)) "iveness" "ive",
  Rule (requireM (>0)) "fulness" "ful",
  Rule (requireM (>0)) "ousness" "ous",
  Rule (requireM (>0)) "iviti" "ive",
  Rule (requireM (>0)) "biliti" "ble"]


step2 :: String -> String
step2 = applyStep porterStep2


-- Step 3

porterStep3 :: [Rule]
porterStep3 = [
  Rule (requireM (>0)) "icate" "ic",
  Rule (requireM (>0)) "ative" "",
  Rule (requireM (>0)) "alize" "al",
  Rule (requireM (>0)) "iciti" "ic",
  Rule (requireM (>0)) "ical" "ic",
  Rule (requireM (>0)) "ful" "",
  Rule (requireM (>0)) "ness" ""]


step3 :: String -> String
step3 = applyStep porterStep3


-- Step 4

porterStep4 :: [Rule]
porterStep4 = [
  Rule (requireM (>1)) "al" "",
  Rule (requireM (>1)) "ance" "",
  Rule (requireM (>1)) "ence" "",
  Rule (requireM (>1)) "er" "",
  Rule (requireM (>1)) "ic" "",
  Rule (requireM (>1)) "able" "",
  Rule (requireM (>1)) "ible" "",
  Rule (requireM (>1)) "ant" "",
  Rule (requireM (>1)) "ement" "",
  Rule (requireM (>1)) "ment" "",
  Rule (requireM (>1)) "ent" "",
  Rule (\word -> requireM (>1) word && last word `elem` "st") "ion" "",
  Rule (requireM (>1)) "ou" "",
  Rule (requireM (>1)) "ism" "",
  Rule (requireM (>1)) "ate" "",
  Rule (requireM (>1)) "iti" "",
  Rule (requireM (>1)) "ous" "",
  Rule (requireM (>1)) "ive" "",
  Rule (requireM (>1)) "ize" ""]


step4 :: String -> String
step4 = applyStep porterStep4


-- Step 5

step5b :: String -> String
step5b word
  | m > 1 && head lastChars == head (tail lastChars) &&
      fst (head lastChars) == Consonant && snd (head lastChars) == 'l'
    = init word
  | otherwise = word
  where
    types = assignConsonants word
    m = getM types
    lastChars = reverse $ zip types word


step5 :: String -> String
step5 word
  | last word == 'e' && (m > 1 || m == 1 && not endsWithCVC) = step5b $ init word
  | otherwise = step5b word
  where
    wordInit = init word
    m = getM $ assignConsonants wordInit
    endsWithCVC = revTypes == [Consonant, Vowel, Consonant] &&
                  last wordInit `notElem` "wxy"
      where revTypes = take 3 $ reverse $ assignConsonants wordInit


porter :: String -> String
porter "" = ""
porter word = step5 $ step4 $ step3 $ step2 $ step1c $ step1b $ step1a word
