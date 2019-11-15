module TextUtils.Processing (getExcerpt, splitWords, getPositions, filterChars,
  hexEncode, hexDecode) where

import Data.Char
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Binary as B
import Numeric(showHex)
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Char (ord)


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


hexEncode :: ByteString -> String
hexEncode = encodeList . ByteString.unpack
  where
    encodeList :: [B.Word8] -> String
    encodeList [] = ""
    encodeList (word:rest) =
      let a = word `div` 16
          b = word `mod` 16
      in showHex a $ showHex b $ encodeList rest


hexDecode :: String -> Maybe ByteString
hexDecode str = ByteString.pack <$> decodeList str
  where
    decodeList :: String -> Maybe [B.Word8]
    decodeList [] = Just []
    decodeList [_] = Nothing
    decodeList (a:b:rest) = do
      ia <- hexToInt a
      ib <- hexToInt b
      ((fromIntegral $ ia * 16 + ib):) <$> decodeList rest

    hexToInt :: Char -> Maybe Int
    hexToInt x
      | x `elem` ['0'..'9'] = Just $ ord x - ord '0'
      | x `elem` ['a'..'f'] = Just $ ord x - ord 'a' + 10
      | x `elem` ['A'..'F'] = Just $ ord x - ord 'A' + 10
      | otherwise = Nothing
