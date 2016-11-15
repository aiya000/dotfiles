{-# LANGUAGE OverloadedStrings #-}

-- The module for yi helpers
module Yi.MyConfig.Helper
  ( VimEvaluator
  , quitEditorIfModifiedNothing
  , closeWinOrQuitEditor
  , findTagTable
  ) where

import Control.Monad (forM)
import Control.Monad.Extra (ifM)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.List.PointedList (PointedList)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Lens.Micro.Extras (view)
import System.EasyFile (getCurrentDirectory, doesFileExist)
import Yi.Buffer.Misc (BufferId(FileBuffer), identA)
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
import qualified Yi.Tab as T
import qualified Yi.Window as W

-- For debug
import Yi.Debug (initDebug, logPutStrLn)
import Yi.Editor (printMsg)
import qualified Yi.Debug as Debug

-- VimEvaluator generate the action from EventString
type VimEvaluator = V.EventString -> EditorM ()


-- Like quitEditor but checking unsaved buffers
quitEditorIfModifiedNothing :: YiM ()
quitEditorIfModifiedNothing = do
  bufStack        <- gets E.bufferStack
  buffers         <- gets E.buffers
  unsavedBufStack <- forM bufStack $ \buf ->
    ifM (needsSaving buf)
      (return $ M.lookup buf buffers)  -- M.lookup get Just absolutely
      (return Nothing)
  let findJusts xs (Just x) = x : xs
      findJusts xs Nothing  = xs
      unsavedBufferNames    = foldl' findJusts [] unsavedBufStack
  if null unsavedBufferNames
     then quitEditor
     else errorEditor $ "No write since last change for buffer " <> showT unsavedBufferNames

-- If win num greater than 1, do tryCloseE.
-- else, do quitEditorIfModifiedNothing
closeWinOrQuitEditor :: YiM ()
closeWinOrQuitEditor = do
  --myBuffers <- gets $ map W.bufkey . concat . map toList . map (view T.tabWindowsA) . toList . E.tabs_
  myBuffers <- gets $ map W.bufkey . forgot . fmap (view T.tabWindowsA) . E.tabs_
  bufMap    <- gets E.buffers
  let fBuffers    = flip map myBuffers $ \buf -> fromJust (M.lookup buf bufMap)  -- M.lookup get Just absolutely
      fileBuffers = filter isFileBuffer . map (view identA) $ fBuffers
  if length fileBuffers == 1
    then quitEditorIfModifiedNothing
    else withEditor E.tryCloseE
  where
    forgot :: PointedList (PointedList a) -> [a]
    forgot = concat . map toList . toList
    isFileBuffer (FileBuffer _) = True
    isFileBuffer _              = False


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
      x <- sequence [ doesFileExist $ currentDir ++ "/tags"
                    , doesFileExist $ currentDir ++ "/.git/tags" ]
      case x of
        [True, _] -> return . Just $ currentDir ++ "/tags"
        [_, True] -> return . Just $ currentDir ++ "/.git/tags"
        _         -> findTagFile $ depth - 1
