{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)
import Control.Monad.Extra (ifM)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Prototype (override)
import Prelude hiding (foldl)
import Yi.Boot (yi, reload)
import Yi.Buffer.Misc (setVisibleSelection, identString)
import Yi.Config (defaultKm, configUI, configWindowFill)
import Yi.Config.Default (defaultVimConfig)
import Yi.Core (quitEditor, errorEditor)
import Yi.Editor (EditorM, MonadEditor, getEditorDyn, putEditorDyn, closeBufferAndWindowE, withCurrentBuffer)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.File (viWrite, fwriteAllY)
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, pureEval)
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.Ex.Commands.Common (needsSaving)
import Yi.Keymap.Vim.StateUtils (switchModeE, resetCountE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import Yi.Monad (gets)
import Yi.String (showT)
import Yi.Types (YiVariable)
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


--modifyEditorDyn :: (MonadEditor m, YiVariable a, Functor m) => (a -> a) -> m ()
modifyEditorDyn :: (MonadEditor m, YiVariable a) => (a -> a) -> m ()
modifyEditorDyn f = getEditorDyn >>= \s -> putEditorDyn (f s)


-- Like switchModeE, for YiM
switchModeY :: V.VimMode -> YiM ()
switchModeY mode = modifyEditorDyn $ \s -> s { V.vsMode = mode }


-- Entry point
main :: IO ()
main = yi defaultVimConfig
  { defaultKm = myDefaultKm
  , configUI = (configUI defaultVimConfig) { configWindowFill = '~' }
  }


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
  , nnoremapE' " h"  E.prevWinE  -- temporary
  , nnoremapE' " j"  E.nextWinE  -- temporary
  , nnoremapE' " k"  E.prevWinE  -- temporary
  , nnoremapE' " l"  E.nextWinE  -- temporary
  , nnoremapE' "gH"  E.newTabE
  , nnoremapE' "ghh" E.newTabE  -- temporary
  , nnoremapE' "ghq" E.tryCloseE  --TODO
  , nnoremapY' "ghQ" quitEditorWithBufferCheck
  , nnoremapE' "ghc" E.closeBufferAndWindowE
  , nnoremapE' "ghv" (E.splitE >> E.prevWinE)  -- Clone win to right
  --, nnoremapE' "ghs" (E.splitE >> ?)  -- Clone win to under
  , nnoremapE' "gho" E.closeOtherE  -- Do :only
  --, nnoremapE' "g:"  (eval ":buffers<CR>")  --FIXME: doesn't works
  --, nnoremapE' "gh\"" (E.setDividerPosE 0 0.3)
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
    nnoremapY' :: V.EventString -> YiM () -> V.VimBinding
    nnoremapY' key x = mkStringBindingY V.Normal (key, x, id)


-- Keymappings for V.VimMode (∀a. V.Insert a)
insertBindings :: [V.VimBinding]
insertBindings =
  [ inoremapE (keyC 'l') (switchModeE V.Normal)
  , inoremapY' "<C-k><CR>" (viWrite >> switchModeY V.Normal)  -- Yi interpret <C-j> as <CR>
  --, inoremap? "<C-k><C-k>" --TODO: to delete tail
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
