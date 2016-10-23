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
import Yi.Keymap.Vim.Common (EventString)
import Yi.Keymap.Vim.Common (VimMode(Normal), RepeatToken(Drop))
import Yi.Keymap.Vim.Utils (mkBindingE, mkStringBindingE)
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
myBindings =
  [ nmap  (c 'p') S.previousTabE
  , nmap  (c 'n') S.nextTabE
  , nmap' "gH"    S.newTabE
  --, nmap "<Home>" (withCurrentBuffer moveToSol)
  --, nmap "<End>" (withCurrentBuffer moveToEol)
  --, nmap " " (eval ":nohlsearch<CR>")
  ]
  where
    -- like <C-x> key of Vim
    c x = Event (KASCII x) [MCtrl]

    -- like nmap command of Vim
    nmap :: Event -> EditorM () -> VimBinding
    nmap key x = mkBindingE Normal Drop (key, x, id)

    -- like nmap command of Vim from EventString
    nmap' :: EventString -> EditorM () -> VimBinding
    nmap' key x = mkStringBindingE Normal Drop (key, x, id)
    --imap x y = V2.VimBindingE (\evs state -> case V2.vsMode state of
    --                            V2.Insert _ ->
    --                                fmap (const (y >> return V2.Continue))
    --                                     (evs `V2.matchesString` x)
    --                            _ -> V2.NoMatch)
