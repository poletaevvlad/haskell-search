module Pages.UrlUtils(popUrlComponent, setPageNum) where

import Data.Char(isDigit)
import Data.List(isPrefixOf)

popUrlComponent :: String -> String
popUrlComponent "/" = "/"
popUrlComponent url =
  let newUrl = map fst $ takeWhile snd $ zip url $ (tail . mapLastComponent) url
  in if null newUrl then "/" else newUrl
  where
    mapLastComponent :: String -> [Bool]
    mapLastComponent [] = [False]
    mapLastComponent [_] = [False]
    mapLastComponent ('/': xs) = True: mapLastComponent xs
    mapLastComponent (_: xs) = let rest = mapLastComponent xs in (head rest): rest


setPageNum :: String -> Int -> String
setPageNum uri pageNum =
  let result = removedCurrentPage ++ pageNumComponent
  in if null result then "/" else result
  where
    removeSlash str = if head str == '/' then tail str else str

    skipNumbers "" = Nothing
    skipNumbers str =
      if isDigit $ head str then Just $ dropWhile isDigit str else Nothing

    removeNumPrefix "" = Nothing
    removeNumPrefix str =
      if "-egap" `isPrefixOf` str then Just $ drop 5 str else Nothing

    removedCurrentPage =
      let
        reversed = removeSlash $ reverse uri
        path = removeSlash <$> (skipNumbers reversed >>= removeNumPrefix)
      in case path of
        Nothing -> reverse reversed
        Just t -> reverse t
    pageNumComponent = if pageNum == 1 then "" else "/page-" ++ show pageNum
