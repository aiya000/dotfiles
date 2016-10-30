{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Prototype (override)
import Yi.Boot (yi, reload)
import Yi.Buffer.Misc (setVisibleSelection)
import Yi.Config (defaultKm, configUI, configWindowFill)
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor (EditorM, MonadEditor, getEditorDyn, putEditorDyn, closeBufferAndWindowE, withCurrentBuffer)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.File (viWrite)
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings)
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.StateUtils (switchModeE, resetCountE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import qualified Yi.Config.Simple as S
import qualified Yi.Keymap.Vim.Common as V


main :: IO ()
main = yi defaultVimConfig
  { defaultKm = myDefaultKm
  , configUI = (configUI defaultVimConfig) { configWindowFill = '~' }
  }


-- Compose yi's vim keymapping and my keymapping
myDefaultKm :: KeymapSet
myDefaultKm = mkKeymapSet $ defVimConfig `override` \super _ -> super
  { vimBindings = myBindings <> vimBindings super
  }


-- My keymapping
myBindings :: [V.VimBinding]
myBindings = normalBindings ++ insertBindings ++ visualBindings ++ exBindings

-- Like <C-{x}> key of Vim
keyC :: Char -> Event
keyC x = Event (KASCII x) [MCtrl]

-- Keymappings for V.VimMode V.Normal
normalBindings :: [V.VimBinding]
normalBindings =
  [ nnoremapE (keyC 'p')  S.previousTabE
  , nnoremapE (keyC 'n')  S.nextTabE
  , nnoremapE' " h"  S.prevWinE  -- temporary
  , nnoremapE' " j"  S.nextWinE  -- temporary
  , nnoremapE' " k"  S.prevWinE  -- temporary
  , nnoremapE' " l"  S.nextWinE  -- temporary
  , nnoremapE' "gH"  S.newTabE
  , nnoremapE' "ghh" S.newTabE  -- temporary
  , nnoremapE' "ghq" S.tryCloseE
  --, nnoremapE' "ghQ"
  , nnoremapE' "ghc" (S.closeBufferE "")  -- Close current buffer and current window
  , nnoremapE' "ghv" (S.splitE >> S.prevWinE)  -- Clone win to right
  --, nmap' "gh\"" (S.setDividerPosE 0 0.3)
  , nnoremapY' "<C-k><C-r>" reload
  , nnoremapY' "<C-k><CR>" viWrite  -- Yi interpret <C-j> as <CR>
  -- Complete official lost things
  , nnoremapE' "<C-w>w" S.nextWinE
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
    switchModeY :: V.VimMode -> YiM ()
    switchModeY mode = getEditorDyn >>= \s -> putEditorDyn s { V.vsMode = mode }
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
