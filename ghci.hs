module Preset where

import Control.Monad
import Data.Text
import Prelude

lexIt :: (Show failure, Show token) => (Text -> Either failure [(token, pos)]) -> Text -> IO ()
lexIt lexer code =
  case lexer code of
    Left e -> print e
    Right xs -> mapM_ (print . fst) xs

lexItPos :: (Show failure, Show token, Show pos) => (Text -> Either failure [(token, pos)]) -> Text -> IO ()
lexItPos lexer code =
  case lexer code of
    Left e -> print e
    Right xs ->
      forM_ xs $ \(x, pos) -> do
        putStrLn $ (show x) <> "\t| " <> (show pos)
