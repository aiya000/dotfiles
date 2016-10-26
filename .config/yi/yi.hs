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
  [ nmapE (keyC 'p')  S.previousTabE
  , nmapE (keyC 'n')  S.nextTabE
  , nmapE' " h"  S.prevWinE  -- temporary
  , nmapE' " j"  S.nextWinE  -- temporary
  , nmapE' " k"  S.prevWinE  -- temporary
  , nmapE' " l"  S.nextWinE  -- temporary
  , nmapE' "gH"  S.newTabE
  , nmapE' "ghh" S.newTabE  -- temporary
  , nmapE' "ghq" S.tryCloseE
  , nmapE' "ghc" (S.closeBufferE "")  -- Close current buffer and current window
  --, nmap' "gh\"" (S.setDividerPosE 0 0.3)
  , nmapY' "<C-k><C-r>" reload
  -- Complete official lost things
  , nmapE' "<C-w>w" S.nextWinE
  ]
  where
    -- like nnoremap of Vim for EditorM
    nmapE :: Event -> EditorM () -> VimBinding
    nmapE key x = nmapE' (eventToEventString key) x
    -- like nnoremap of Vim for EditorM from EventString
    nmapE' :: EventString -> EditorM () -> VimBinding
    nmapE' key x = mkStringBindingE Normal Drop (key, x, id)
    -- for YiM
    nmapY' :: EventString -> YiM () -> VimBinding
    nmapY' key x = mkStringBindingY Normal (key, x, id)

-- Keymappings for VimMode (∀a. Insert a)
insertBindings :: [VimBinding]
insertBindings =
  [ imap (keyC 'l') (switchModeE Normal)
  --, imap' "<C-k><C-j>"
  ]
  where
    -- like inoremap of Vim
    imap :: Event -> EditorM () -> VimBinding
    imap key x = imap' (eventToEventString key) x
    -- like inoremap of Vim from EventString
    -- for ∀a. Insert a
    imap' :: EventString -> EditorM () -> VimBinding
    imap' key x = VimBindingE $ \evs state ->
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
