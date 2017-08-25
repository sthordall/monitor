module Helpers
  ( snakeOptions
  , log
  ) where

import qualified Data.Aeson.Types as AT
import Data.Aeson (defaultOptions)
import Data.Char (toLower, isUpper)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Prelude hiding (log)

snakeIt :: String -> String
snakeIt = change ""
  where
    change :: String -> String -> String
    -- TODO: improve it to avoid repetitive "_" signs with abbreviations
    change aux [] = aux
    change aux (x:xs)
      | isUpper x = change (aux ++ "_" ++ [toLower x]) xs
      | otherwise = change (aux ++ [toLower x]) xs

snakeOptions :: AT.Options
snakeOptions = defaultOptions { AT.fieldLabelModifier = snakeIt }

log :: String -> IO ()
log msg = do
  now <- formatTime defaultTimeLocale "[%H:%M:%S] " <$> getCurrentTime
  putStrLn $ now ++ msg

