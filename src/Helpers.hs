module Helpers
  ( snakeOptions
  ) where

import qualified Data.Aeson.Types as AT
import Data.Aeson (defaultOptions)
import Data.Char (toLower, isUpper)

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
