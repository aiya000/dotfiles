{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Prototype (override)
import Yi.Boot (yi)
import Yi.Config (Config(defaultKm))
import Yi.Config.Default (defaultVimConfig)
import Yi.Config.Simple (EditorM)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.Keymap (KeymapSet)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, VimBinding)
import Yi.Keymap.Vim.Common (VimMode(Normal,Insert), VimBinding(VimBindingE), RepeatToken(Drop,Continue), EventString, vsMode, matchesString, MatchResult(NoMatch))
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.StateUtils (switchModeE)
import Yi.Keymap.Vim.Utils (mkStringBindingE)
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

normalBindings :: [VimBinding]
normalBindings =
  [ nmap (keyC 'p')  S.previousTabE
  , nmap (keyC 'n')  S.nextTabE
  , nmap' " h"  S.prevWinE  -- temporary
  , nmap' " j"  S.nextWinE  -- temporary
  , nmap' " k"  S.prevWinE  -- temporary
  , nmap' " l"  S.nextWinE  -- temporary
  , nmap' "gH"  S.newTabE
  , nmap' "ghh" S.newTabE  -- temporary
  , nmap' "ghq" S.tryCloseE
  , nmap' "ghc" (S.closeBufferE "")
  , nmap' "gh\"" (S.setDividerPosE 1 0.3)
  , nmap' "<C-k><C-r>" S.nextWinE
  -- Complete official lost things
  , nmap' "<C-w>w" S.nextWinE
  --, nmap "<Home>" (withCurrentBuffer moveToSol)
  --, nmap "<End>" (withCurrentBuffer moveToEol)
  --, nmap " " (eval ":nohlsearch<CR>")
  ]
  where
    -- like nnoremap command of Vim
    nmap :: Event -> EditorM () -> VimBinding
    nmap key x = nmap' (eventToEventString key) x
    -- like nnoremap command of Vim from EventString
    nmap' :: EventString -> EditorM () -> VimBinding
    nmap' key x = mkStringBindingE Normal Drop (key, x, id)

insertBindings :: [VimBinding]
insertBindings =
  [ imap (keyC 'l') (switchModeE Normal)
  ]
  where
    -- like inoremap command of Vim
    imap :: Event -> EditorM () -> VimBinding
    imap key x = imap' (eventToEventString key) x
    -- like inoremap command of Vim from EventString
    -- for ∀a. Insert a
    imap' :: EventString -> EditorM () -> VimBinding
    imap' key x = VimBindingE $ \evs state ->
      case vsMode state of
        Insert _ -> fmap (const $ x >> return Continue) (evs `matchesString` key)
        _        -> NoMatch

visualBindings :: [VimBinding]
visualBindings = []

exBindings :: [VimBinding]
exBindings = []
