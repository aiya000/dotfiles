{-# LANGUAGE OverloadedStrings #-}

-- The module for yi helpers
module Yi.MyConfig.Helper
 ( VimEvaluator
 , keyC
 , quitEditorWithBufferCheck
 , closeWinOrQuitEditor
 , switchModeY
 ) where
 
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.Keymap (YiM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V
import Yi.Monad (gets)
import Control.Monad.Extra (ifM)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Data.List (foldl')
import Yi.Core (quitEditor, errorEditor)
import Data.Monoid ((<>))
import Yi.String (showT)
import Yi.Editor (EditorM, withEditor, getEditorDyn, putEditorDyn, withCurrentBuffer, printMsg)
import Control.Monad (forM)
import Yi.Buffer.Misc (identString)
import Data.Maybe (fromJust)

-- VimEvaluator generate the action from EventString
type VimEvaluator = V.EventString -> EditorM ()


-- Like <C-{x}> key of Vim
keyC :: Char -> Event
keyC x = Event (KASCII x) [MCtrl]

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


-- Like switchModeE, for YiM
switchModeY :: V.VimMode -> YiM ()
switchModeY mode = getEditorDyn >>= \s -> putEditorDyn s { V.vsMode = mode }
