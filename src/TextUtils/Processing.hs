module TextUtils.Processing (getExcerpt, splitWords, getPositions,
  filterChars) where

import Data.Char
import Data.Map(Map)
import qualified Data.Map as M


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
    isWordSeparator char = char /= '\'' && (isSeparator char || isPunctuation char || isControl char)

    helper :: [String] -> String -> [String]
    helper [] _ = error "illegal state"
    helper (curr:pr) [] = reverse curr:pr
    helper (curr:pr) (c:cs)
      | isWordSeparator c = helper ("":reverse curr:pr) cs
      | otherwise = helper ((c:curr):pr) cs


getPositions :: (Ord a) => [a] -> [(a, [Int])]
getPositions string =
  map (\(c, v) -> (c, reverse v)) $ M.toList $ getPos string 0 M.empty
  where
    getPos :: (Ord a) => [a] -> Int -> Map a [Int] -> Map a [Int]
    getPos [] _ m = m
    getPos (c: cs) i m = let value = i : M.findWithDefault [] c m
                       in getPos cs (i + 1) $ M.insert c value m


filterChars :: String -> String
filterChars = concatMap lowerLatin
  where
    lowerLatin x
      | x `elem` ['a'..'z'] = [x]
      | x `elem` ['A'..'Z'] = [toLower x]
      | x == '\'' = []
      | otherwise = [' ']

