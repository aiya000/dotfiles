{-# LANGUAGE OverloadedStrings #-}

-- The module for yi helpers
module Yi.MyConfig.Helper
 ( VimEvaluator
 , keyC
 , quitEditorWithBufferCheck
 , closeWinOrQuitEditor
 , switchModeY
 , viewRegister
 ) where

import Control.Monad (forM, void)
import Control.Monad.Extra (ifM)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Yi.Buffer.Misc (identString)
import Yi.Core (quitEditor, errorEditor)
import Yi.Editor (EditorM, withEditor, getEditorDyn, putEditorDyn, newBufferE)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.Keymap (YiM)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Yi.Monad (gets)
import Yi.Rope (YiString)
import Yi.String (showT)
import Yi.Types (BufferId(MemBuffer))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Rope as R

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

-- Show registered register and content in new buffer
viewRegister :: EditorM ()
viewRegister = do
  xs <- HM.toList . V.vsRegisterMap <$> getEditorDyn
  let xs'       = visualizeConvert xs
      registers = flip map xs' $ \(name, content) -> "\"" <> R.singleton name <> " | " <> content <> "\n"
      bufDetail = "--- Register ---\n" <> R.concat registers
  void $ newBufferE (MemBuffer "Register list") bufDetail
  where
    replaceName n | n == '\NUL' = '"'
                  | otherwise   = n
    replaceContent = let replaceContentChar c | c == '\n' = "^J"
                                              | otherwise = [c]
                     in concatMap replaceContentChar
    visualizeConvert :: [(V.RegisterName, V.Register)] -> [(V.RegisterName, YiString)]
    visualizeConvert = map $ \(name, reg) ->
      let content = R.toString . V.regContent $ reg
      in ( replaceName name
         , R.fromString . replaceContent $ content
         )
