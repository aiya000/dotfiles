{-# LANGUAGE OverloadedStrings #-}

-- The module for yi helpers
module Yi.MyConfig.Helper
  ( VimEvaluator
  , quitEditorWithBufferCheck
  , closeWinOrQuitEditor
  , findTagTable
  ) where

import Control.Monad (forM)
import Control.Monad.Extra (ifM)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import System.EasyFile (getCurrentDirectory, doesFileExist)
import Yi.Buffer.Misc (identString)
import Yi.Core (quitEditor, errorEditor)
import Yi.Editor (EditorM, withEditor)
import Yi.Keymap (YiM)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Yi.Monad (gets)
import Yi.String (showT)
import Yi.Tag (TagTable, importTagTable)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V

-- For debug
--import Yi.Debug (initDebug, logPutStrLn)
--import Yi.String (showT)

-- VimEvaluator generate the action from EventString
type VimEvaluator = V.EventString -> EditorM ()


-- Like quitEditor but checking unsaved buffers
quitEditorWithBufferCheck :: YiM ()
quitEditorWithBufferCheck = do
  bufStack        <- gets E.bufferStack
  buffers         <- gets E.buffers
  unsavedBufStack <- forM bufStack $ \buf ->
    ifM (needsSaving buf)
      (return . Just . identString . fromJust $ M.lookup buf buffers)
      (return Nothing)
  let findJusts xs (Just x) = x : xs
      findJusts xs Nothing  = xs
      unsavedBufferNames    = foldl' findJusts [] unsavedBufStack
  if null unsavedBufferNames
     then quitEditor
     else errorEditor $ "No write since last change for buffer " <> showT unsavedBufferNames

-- If win num greater than 1, do tryCloseE.
-- else, do quitEditorWithBufferCheck
closeWinOrQuitEditor :: YiM ()
closeWinOrQuitEditor =
  ifM (gets $ (2<=) . NE.length . E.bufferStack)
    (withEditor E.tryCloseE)
    quitEditorWithBufferCheck


-- Find ctag file and Load it
findTagTable :: IO (Maybe TagTable)
findTagTable = do
  mayTagFile <- findTagFile maxDepth
  case mayTagFile of
    Nothing      -> return Nothing
    Just tagFile -> Just <$> importTagTable tagFile
  where
    -- findTagTable should search tag file {maxDepth} times
    maxDepth = 5
    findTagFile :: Int -> IO (Maybe FilePath)
    findTagFile 0     = return Nothing
    findTagFile depth = do
      let parentsSuffix = foldl' (++) "" . replicate (maxDepth - depth) $ "/.."
      currentDir <- (++) <$> getCurrentDirectory <*> pure parentsSuffix
      --TODO: Implement for windows path
      ifM (doesFileExist $ currentDir ++ "/tags")
        (return . Just $ currentDir ++ "/tags") $
        ifM (doesFileExist $ currentDir ++ "/.git/tags")
          (return . Just $ currentDir ++ "/.git/tags")
          (findTagFile $ depth - 1)
