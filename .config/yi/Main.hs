{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)
import Control.Monad.Extra (ifM)
import Control.Monad.State.Lazy (execStateT)
import Data.List (foldl')
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Prototype (override)
import Lens.Micro.Platform ((.=))
import Prelude hiding (foldl)
import System.Environment (getArgs)
import Yi.Boot (yi, reload)
import Yi.Buffer.Misc (setVisibleSelection, identString)
import Yi.Config (defaultKm, configUI, configWindowFill)
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Lens (defaultKmA, configUIA, startActionsA)
import Yi.Config.Simple (configMain)
import Yi.Config.Simple.Types (ConfigM, runConfigM)
import Yi.Core (startEditor, quitEditor, errorEditor, refreshEditor)
import Yi.Editor (EditorM, MonadEditor, withEditor, getEditorDyn, putEditorDyn, closeBufferAndWindowE, withCurrentBuffer)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.File (openNewFile)
import Yi.File (viWrite, fwriteAllY)
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Emacs.KillRing (killLine)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, pureEval)
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Yi.Keymap.Vim.StateUtils (switchModeE, resetCountE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import Yi.Monad (gets)
import Yi.String (showT)
import Yi.Types (Action(YiA,EditorA), YiVariable)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V

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


--modifyEditorDyn :: (MonadEditor m, YiVariable a, Functor m) => (a -> a) -> m ()
modifyEditorDyn :: (MonadEditor m, YiVariable a) => (a -> a) -> m ()
modifyEditorDyn f = getEditorDyn >>= \s -> putEditorDyn (f s)


-- Like switchModeE, for YiM
switchModeY :: V.VimMode -> YiM ()
switchModeY mode = modifyEditorDyn $ \s -> s { V.vsMode = mode }


-- Entry point
main :: IO ()
main = do
    files  <- getArgs
    let openFileActions = intersperse (EditorA E.newTabE) $ map (YiA . openNewFile) files
    config <- flip execStateT defaultConfig . runConfigM $ do
      myConfig
      startActionsA .= openFileActions
    startEditor config Nothing

myConfig :: ConfigM ()
myConfig = do
  configureVty
  configureMyVim
  configureHaskellMode
  configureMiscModes

configureMyVim :: ConfigM ()
configureMyVim = do
  configureVim
  defaultKmA .= myDefaultKm
  configUIA  .= (configUI defaultConfig) { configWindowFill = '~' }


-- Compose yi's vim keymapping and my keymapping
myDefaultKm :: KeymapSet
myDefaultKm = mkKeymapSet $ defVimConfig `override` \super this ->
  let eval = pureEval this
  in super
    { vimBindings = myBindings eval ++ vimBindings super
    }


-- My keymapping
myBindings :: VimEvaluator -> [V.VimBinding]
myBindings eval = normalBindings eval ++ insertBindings ++ visualBindings ++ exBindings

-- Keymappings for V.VimMode V.Normal
normalBindings :: VimEvaluator -> [V.VimBinding]
normalBindings _ =
  [ nnoremapE (keyC 'p') E.previousTabE
  , nnoremapE (keyC 'n') E.nextTabE
  , nnoremapY (keyC 'l') refreshEditor
  , nnoremapE' " h"  E.prevWinE  -- temporary
  , nnoremapE' " j"  E.nextWinE  -- temporary
  , nnoremapE' " k"  E.prevWinE  -- temporary
  , nnoremapE' " l"  E.nextWinE  -- temporary
  , nnoremapE' "gH"  E.newTabE
  , nnoremapE' "ghh" E.newTabE  -- temporary
  , nnoremapY' "ghq" closeWinOrQuitEditor
  , nnoremapY' "ghQ" quitEditorWithBufferCheck
  , nnoremapE' "ghc" E.closeBufferAndWindowE
  , nnoremapE' "ghv" (E.splitE >> E.prevWinE)  -- Clone win to right
  --, nnoremapE' "ghs" (E.splitE >> ?)  -- Clone win to under
  , nnoremapE' "gho" E.closeOtherE  -- Do :only
  --, nnoremapE' "g:"  (eval ":buffers<CR>")  --FIXME: doesn't works
  , nnoremapE' "gh\"" (resizeCurrentWin 3)
  , nnoremapY' "<C-k><C-r>" reload
  , nnoremapY' "<C-k><CR>"  viWrite  -- Yi interpret <C-j> as <CR>
  , nnoremapY' "<C-k>J"     (fwriteAllY >> return ())
  --, nnoremapE' "<C-k><C-l>" (eval ":nohlsearch<CR>")  --FIXME: doesn't works correctly
  -- Complete official lost things
  , nnoremapE' "<C-w>w" E.nextWinE
  ]
  where
    -- Like nnoremap of Vim for EditorM
    nnoremapE :: Event -> EditorM () -> V.VimBinding
    nnoremapE key x = nnoremapE' (eventToEventString key) x
    -- Like nnoremap of Vim for EditorM from V.EventString
    nnoremapE' :: V.EventString -> EditorM () -> V.VimBinding
    nnoremapE' key x = mkStringBindingE V.Normal V.Drop (key, x, id)
    -- for YiM
    nnoremapY  :: Event -> YiM () -> V.VimBinding
    nnoremapY  key x = nnoremapY' (eventToEventString key) x
    nnoremapY' :: V.EventString -> YiM () -> V.VimBinding
    nnoremapY' key x = mkStringBindingY V.Normal (key, x, id)

    resizeCurrentWin :: Int -> EditorM ()
    resizeCurrentWin lineNum = undefined


-- Keymappings for V.VimMode (∀a. V.Insert a)
insertBindings :: [V.VimBinding]
insertBindings =
  [ inoremapE (keyC 'l') (switchModeE V.Normal)
  , inoremapY' "<C-k><CR>" (viWrite >> switchModeY V.Normal)  -- Yi interpret <C-j> as <CR>
  , inoremapY' "<C-k><C-k>" (killLine Nothing)
  ]
  where
    -- The keymapping implementor for both of V.VimBindingY and V.VimBindingE
    implBinding :: MonadEditor m => V.EventString -> m () -> V.EventString -> V.VimState -> V.MatchResult (m V.RepeatToken)
    implBinding key context = \key' state ->
      case V.vsMode state of
        V.Insert _ -> (const $ context >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch
    -- Like inoremap of Vim from Event for EditorM
    inoremapE :: Event -> EditorM () -> V.VimBinding
    inoremapE key x = inoremapE' (eventToEventString key) x
    -- Like inoremap of Vim from V.EventString for EditorM
    -- for ∀a. V.Insert a
    inoremapE' :: V.EventString -> EditorM () -> V.VimBinding
    inoremapE' key x = V.VimBindingE $ implBinding key x
    -- Like inoremap of Vim from V.EventString for YiM
    -- for ∀a. V.Insert a
    inoremapY' :: V.EventString -> YiM () -> V.VimBinding
    inoremapY' key x = V.VimBindingY $ implBinding key x

-- Keymapping for V.VimMode (∀a V.Visual a)
visualBindings :: [V.VimBinding]
visualBindings =
  [ vnoremapE (keyC 'l') exitVisual
  ]
  where
    -- The keymappings implementor for both of V.VimBindingY and V.VimBindingE
    implBinding :: MonadEditor m => V.EventString -> m () -> V.EventString -> V.VimState -> V.MatchResult (m V.RepeatToken)
    implBinding key context = \key' state ->
      case V.vsMode state of
        V.Visual _ -> (const $ context >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch
    -- Like vnoremap of Vim for EditorM
    vnoremapE :: Event -> EditorM () -> V.VimBinding
    vnoremapE key x = vnoremapE' (eventToEventString key) x
    -- Like vnoremap of Vim for EditorM from V.EventString
    vnoremapE' :: V.EventString -> EditorM () -> V.VimBinding
    vnoremapE' key x = V.VimBindingE $ implBinding key x

    -- See https://www.stackage.org/haddock/lts-7.4/yi-0.12.6/src/Yi.Keymap.Vim.ExMap.html#exitEx
    exitVisual = do
      resetCountE
      switchModeE V.Normal
      withCurrentBuffer $ setVisibleSelection False

-- Keymappings for V.VimMode V.Ex
exBindings :: [V.VimBinding]
exBindings =
  [ cnoremap (keyC 'l') exitEx'
  ]
  where
    -- Like cnoremap of Vim
    cnoremap :: Event -> EditorM () -> V.VimBinding
    cnoremap key x = cnoremap' (eventToEventString key) x
    -- Like cnoremap of Vim from V.EventString
    cnoremap' :: V.EventString -> EditorM () -> V.VimBinding
    cnoremap' key x = mkStringBindingE V.Ex V.Finish (key, x, id)

    -- See https://www.stackage.org/haddock/lts-7.4/yi-0.12.6/src/Yi.Keymap.Vim.ExMap.html#exitEx
    exitEx' = do
      resetCountE
      switchModeE V.Normal
      closeBufferAndWindowE
      withCurrentBuffer $ setVisibleSelection False
