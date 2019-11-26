{-# LANGUAGE OverloadedStrings #-}

module Presentation.DocEditor(docEditor, EditorType(..)) where

import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


data EditorType = Create | Edit
  deriving (Eq)


docEditor :: EditorType -> String -> String -> H.Html
docEditor edType title text = do
  H.form ! A.class_ "edit-form" ! A.method "POST" $ do
    H.div ! A.class_ "form-row" $ do
      H.label "Title:" ! A.for "title"
      H.input ! A.type_ "text" ! A.name "title" ! A.value (toValue title)
    H.textarea (H.toHtml text) ! A.name "text"
    let btnText = if edType == Create then "[ Publish ]" else "[ Update ]"
    H.input ! A.type_ "submit" ! A.value btnText
