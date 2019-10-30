module Pages.UrlUtils(popUrlComponent) where

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
