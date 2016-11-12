{-# LANGUAGE OverloadedStrings #-}

-- The module for yi helpers
module Yi.MyConfig.Helper
  ( VimEvaluator
  , quitEditorWithBufferCheck
  , closeWinOrQuitEditor
  --, findTagTable
  ) where

import Control.Monad (forM)
import Control.Monad.Extra (ifM)
import Data.List (foldl', foldl1')
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import System.EasyFile (getCurrentDirectory, getDirectoryContents)
import Yi.Buffer.Misc (identString)
import Yi.Core (quitEditor, errorEditor)
import Yi.Editor (EditorM, withEditor)
import Yi.Keymap (YiM)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Yi.Monad (gets)
import Yi.String (showT)
import Yi.Tag (TagTable)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V

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


