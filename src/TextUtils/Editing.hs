module TextUtils.Editing(toEditor) where

import Data.List.Split(splitWhen)
import Data.Char(isSpace)


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

