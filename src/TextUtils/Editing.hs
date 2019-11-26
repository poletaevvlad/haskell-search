module TextUtils.Editing(toEditor, fromEditor, removeSpaces) where

import Data.List.Split(splitWhen)
import Data.Char(isSpace)


removeSpaces :: String -> String
removeSpaces "" = ""
removeSpaces text =
  let res = removeSpaces' False text
  in if (not $ null res) && (isSpace $ head res) then tail res else res
  where
    removeSpaces' :: Bool -> String -> String
    removeSpaces' _ "" = ""
    removeSpaces' addSpace (c:rest)
      | isSpace c = removeSpaces' True rest
      | otherwise = if addSpace then ' ':c:(removeSpaces' False rest)
                                else c:(removeSpaces' False rest)


toEditor :: [String] -> Int -> String
toEditor [] _ = ""
toEditor [line] width = formatString True width $ splitWhen isSpace line
  where
    formatString :: Bool -> Int -> [String] -> String
    formatString _ _ [] = []
    formatString newLine lineWidth (word: words) =
      let len = length word
      in if newLine
        then word ++ formatString False (lineWidth - len) words
        else if len < lineWidth
          then " " ++ word ++ formatString False (lineWidth - 1 - len) words
          else "\n" ++ formatString True width (word:words)
toEditor (line:rest) width = (toEditor [line] width) ++ "\n\n" ++ toEditor rest width


fromEditor :: String -> [String]
fromEditor text = reverse $ filter (not . null) $ map (removeSpaces . reverse) $ splitIntoLines [""] text
  where
    splitIntoLines :: [String] -> String -> [String]
    splitIntoLines [] _ = error "Illegal state"
    splitIntoLines text "" = text
    splitIntoLines text ('\n':'\n':rest) = splitIntoLines ("":text) rest
    splitIntoLines (first:text) ('\n':rest) = splitIntoLines ((' ':first):text) rest
    splitIntoLines (first:text) (c:rest) = splitIntoLines ((c:first):text) rest

