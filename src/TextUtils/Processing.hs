module TextUtils.Processing (getExcerpt, splitWords) where

import Data.Char


getExcerpt :: Int -> String -> String
getExcerpt 0 _ = []
getExcerpt _ [] = []
getExcerpt len text = nonLetters ++ word ++ (getExcerpt (len - 1) rest)
  where
    (nonLetters, letters) = dropWhileBoth (not . isLetter) text
    (word, rest) = dropWhileBoth isLetter letters

    dropWhileBoth :: (Char -> Bool) -> String -> (String, String)
    dropWhileBoth _ [] = ([], [])
    dropWhileBoth predicate string@(c: cs)
      | predicate c =
          let (v, next) = dropWhileBoth predicate cs
          in (c:v, next)
      | otherwise = ([], string)


splitWords :: String -> [String]
splitWords text = filter (not . null) $ reverse $ helper [""] text
  where
    isWordSeparator :: Char -> Bool
    isWordSeparator char = isSeparator char || isPunctuation char

    helper :: [String] -> String -> [String]
    helper [] _ = error "illegal state"
    helper (curr:pr) [] = reverse curr:pr
    helper (curr:pr) (c:cs)
      | c == '\'' = helper (curr:pr) cs
      | isWordSeparator c = helper ("":reverse curr:pr) cs
      | otherwise = helper ((c:curr):pr) cs
