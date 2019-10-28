module Database.Processing (getExcerpt) where

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

