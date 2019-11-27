module Presentation.Toolbar(ToolbarAction(..), toolbar) where

import Text.Blaze ((!))
import Text.Blaze(toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.List(intersperse)

data ToolbarAction = Action String String
                   | ConfirmAction String String String


toolbar :: [ToolbarAction] -> H.Html
toolbar actions = H.div ! A.class_ (toValue "toolbar") $ do
  foldl1 (>>) $ intersperse (H.toHtml " - ") $ map renderAction actions
  where
    renderAction :: ToolbarAction -> H.Html
    renderAction (Action url name) = H.a (H.toHtml name) ! A.href (toValue url)
    renderAction (ConfirmAction url name confirm) =
      let js = "return confirm(\"" ++ (escapeJSString confirm) ++ "\")"
      in H.a (H.toHtml name) ! A.href (toValue url) ! A.onclick (toValue js)

    escapeJSString :: String -> String
    escapeJSString "" = ""
    escapeJSString ('\\': rest) = '\\':'\\':(escapeJSString rest)
    escapeJSString ('\"': rest) = '\\':'\"':(escapeJSString rest)
    escapeJSString (c: rest) = c:(escapeJSString rest)
