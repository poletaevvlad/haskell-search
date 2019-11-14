{-# LANGUAGE OverloadedStrings #-}

module Presentation.Layout(appLayout, paginator) where


import Text.Blaze ((!))
import Text.Blaze(string, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


headerLayout :: String -> H.Html
headerLayout query =
  H.div ! A.class_ "header" $ do
    H.a "" ! A.href "/" ! A.class_ "logo"
    H.div ! A.class_ "search-box" $ do
      H.div "Search:"
      H.form ! A.class_ "search-field" ! A.action "/search" ! A.method "GET" $ do
        H.input ! A.type_ "text" ! A.name "q" ! A.class_ "search" ! A.placeholder "Type your request here and press Enter" ! A.value (H.toValue query)


appLayout :: String -> String -> H.Html -> H.Html
appLayout title query content =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml title)
      H.meta ! A.charset "utf-8"
      H.link ! A.rel "stylesheet" ! A.href "/static/styles.css"
    H.body $ do
      H.div ! A.class_ "document" $ do
        headerLayout query
        content


paginator :: (Int -> String) -> Int -> Int -> H.Html
paginator urlFactory page pagesCount
  | pagesCount <= 1 = mempty
  | otherwise = let
      pageIndicator = string (show page ++ "/" ++ show pagesCount)
      paginatorButton :: String -> Int -> H.Html
      paginatorButton label pageIndex
        | pageIndex < 1 || pageIndex > pagesCount = mempty
        | otherwise = H.a (string label) ! A.href (toValue $ urlFactory pageIndex)
    in H.div ! A.class_ "paginator" $ do
      paginatorButton "Previous page" (page - 1)
      " -- "
      pageIndicator
      " -- "
      paginatorButton "Next page" (page + 1)
