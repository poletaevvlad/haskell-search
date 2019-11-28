module Service.Config (parsePort) where

import Data.Text
import Text.Read (readMaybe)


parsePort :: Text -> Either String Int
parsePort text = case readMaybe $ unpack text of
  Nothing -> Left "Port number must be an integer"
  Just port ->
    if port < 0 || port > 0xFFFF
      then Left "Port number must be in range 1..65535"
      else Right port

