{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Prototype (override)
import Yi.Boot (yi, reload)
import Yi.Config (Config(defaultKm))
import Yi.Config.Default (defaultVimConfig)
import Yi.Config.Simple (EditorM)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, VimBinding)
import Yi.Keymap.Vim.Common (VimMode(Normal,Insert,Ex), VimBinding(VimBindingE), RepeatToken(Drop,Continue,Finish), EventString, vsMode, matchesString, MatchResult(NoMatch))
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.StateUtils (switchModeE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import qualified Yi.Config.Simple as S


main :: IO ()
main = yi defaultVimConfig
  { defaultKm = myDefaultKm
  }


-- Compose yi's vim keymapping and my keymapping
myDefaultKm :: KeymapSet
myDefaultKm = mkKeymapSet $ defVimConfig `override` \super _ -> super
  { vimBindings = myBindings <> vimBindings super
  }


-- My keymapping
myBindings :: [VimBinding]
myBindings = normalBindings ++ insertBindings ++ visualBindings ++ exBindings

-- like <C-{x}> key of Vim
keyC :: Char -> Event
keyC x = Event (KASCII x) [MCtrl]

-- Keymappings for VimMode Normal
normalBindings :: [VimBinding]
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
  , nnoremapE' "ghc" (S.closeBufferE "")  -- Close current buffer and current window
  --, nmap' "gh\"" (S.setDividerPosE 0 0.3)
  , nnoremapY' "<C-k><C-r>" reload
  -- Complete official lost things
  , nnoremapE' "<C-w>w" S.nextWinE
  ]
  where
    -- like nnoremap of Vim for EditorM
    nnoremapE :: Event -> EditorM () -> VimBinding
    nnoremapE key x = nnoremapE' (eventToEventString key) x
    -- like nnoremap of Vim for EditorM from EventString
    nnoremapE' :: EventString -> EditorM () -> VimBinding
    nnoremapE' key x = mkStringBindingE Normal Drop (key, x, id)
    -- for YiM
    nnoremapY' :: EventString -> YiM () -> VimBinding
    nnoremapY' key x = mkStringBindingY Normal (key, x, id)

-- Keymappings for VimMode (∀a. Insert a)
insertBindings :: [VimBinding]
insertBindings =
  [ inoremap (keyC 'l') (switchModeE Normal)
  --, inoremap' "<C-k><C-j>"
  ]
  where
    -- like inoremap of Vim
    inoremap :: Event -> EditorM () -> VimBinding
    inoremap key x = inoremap' (eventToEventString key) x
    -- like inoremap of Vim from EventString
    -- for ∀a. Insert a
    inoremap' :: EventString -> EditorM () -> VimBinding
    inoremap' key x = VimBindingE $ \evs state ->
      case vsMode state of
        Insert _ -> fmap (const $ x >> return Continue) (evs `matchesString` key)
        _        -> NoMatch

visualBindings :: [VimBinding]
visualBindings = []

-- Keymappings for VimMode Ex
exBindings :: [VimBinding]
exBindings =
  [ --cmap (keyC 'l') (switchModeE Normal) -- !?
  ]
  where
    -- like cnoremap of Vim
    cmap :: Event -> EditorM () -> VimBinding
    cmap key x = cmap' (eventToEventString key) x
    -- like cnoremap of Vim from EventString
    cmap' :: EventString -> EditorM () -> VimBinding
    cmap' key x = mkStringBindingE Ex Finish (key, x, id)
