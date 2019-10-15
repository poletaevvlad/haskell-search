{-# LANGUAGE OverloadedStrings #-}

module Presentation.Layout(appLayout, paginator) where


import Text.Blaze ((!))
import Text.Blaze(string, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


headerLayout :: H.Html
headerLayout = 
    H.div ! A.class_ "header" $ do
        H.a "" ! A.href "/" ! A.class_ "logo"
        H.div ! A.class_ "search-box" $ do
            H.div "Search:"
            H.form ! A.class_ "search-field" ! A.action "/" ! A.method "POST" $ do
                H.input ! A.type_ "text" ! A.name "q" ! A.class_ "search" ! A.placeholder "Type your request here and press Enter"


appLayout :: String -> H.Html -> H.Html
appLayout title content = 
    H.docTypeHtml $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.charset "utf-8"
            H.link ! A.rel "stylesheet" ! A.href "/static/styles.css"
        H.body $ do
            H.div ! A.class_ "document" $ do
                headerLayout
                content


paginator :: (Int -> String) -> Int -> Int -> H.Html
paginator urlFactory page pagesCount 
  | pagesCount <= 1 = mempty
  | otherwise = let 
      pageIndicator = string (show (page + 1) ++ "/" ++ show pagesCount)
      
      paginatorButton :: String -> Int -> H.Html
      paginatorButton label pageIndex
        | pageIndex < 0 || pageIndex >= pagesCount = mempty
        | otherwise = H.a (string label) ! A.href (toValue $ urlFactory pageIndex)

    in H.div ! A.class_ "paginator" $ do
        paginatorButton "Previous page" (page - 1)
        " -- "
        pageIndicator
        " -- "
        paginatorButton "Next page" (page + 1)