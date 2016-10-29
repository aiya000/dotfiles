{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Prototype (override)
import Yi.Boot (yi, reload)
import Yi.Config (defaultKm, configUI, configWindowFill)
import Yi.Config.Default (defaultVimConfig)
import Yi.Config.Simple (EditorM)
import Yi.Event (Event(Event), Key(KASCII), Modifier(MCtrl))
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings)
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.StateUtils (switchModeE)
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

-- like <C-{x}> key of Vim
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
  -- Complete official lost things
  , nnoremapE' "<C-w>w" S.nextWinE
  ]
  where
    -- like nnoremap of Vim for EditorM
    nnoremapE :: Event -> EditorM () -> V.VimBinding
    nnoremapE key x = nnoremapE' (eventToEventString key) x
    -- like nnoremap of Vim for EditorM from V.EventString
    nnoremapE' :: V.EventString -> EditorM () -> V.VimBinding
    nnoremapE' key x = mkStringBindingE V.Normal V.Drop (key, x, id)
    -- for YiM
    nnoremapY' :: V.EventString -> YiM () -> V.VimBinding
    nnoremapY' key x = mkStringBindingY V.Normal (key, x, id)

-- Keymappings for V.VimMode (∀a. V.Insert a)
insertBindings :: [V.VimBinding]
insertBindings =
  [ inoremap (keyC 'l') (switchModeE V.Normal)
  --, inoremap' "<C-k><C-j>"
  ]
  where
    -- like inoremap of Vim
    inoremap :: Event -> EditorM () -> V.VimBinding
    inoremap key x = inoremap' (eventToEventString key) x
    -- like inoremap of Vim from V.EventString
    -- for ∀a. V.Insert a
    inoremap' :: V.EventString -> EditorM () -> V.VimBinding
    inoremap' key x = V.VimBindingE $ \key' state ->
      case V.vsMode state of
        V.Insert _ -> (const $ x >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch

-- Keymapping for V.VimMode (∀a V.Visual a)
visualBindings :: [V.VimBinding]
visualBindings =
  [ vnoremapE (keyC 'l') (switchModeE V.Normal)  --FIXME: visual drawing is too later
  ]
  where
    -- like vnoremap of Vim for EditorM
    vnoremapE :: Event -> EditorM () -> V.VimBinding
    vnoremapE key x = vnoremapE' (eventToEventString key) x
    -- like vnoremap of Vim for EditorM from V.EventString
    vnoremapE' :: V.EventString -> EditorM () -> V.VimBinding
    vnoremapE' key x = V.VimBindingE $ \key' state ->
      case V.vsMode state of
        V.Visual _ -> (const $ x >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch

-- Keymappings for V.VimMode V.Ex
exBindings :: [V.VimBinding]
exBindings =
  [ cnoremap (keyC 'l') (switchModeE V.Normal) --FIXME: cannot escape from ex area
  ]
  where
    -- like cnoremap of Vim
    cnoremap :: Event -> EditorM () -> V.VimBinding
    cnoremap key x = cnoremap' (eventToEventString key) x
    -- like cnoremap of Vim from V.EventString
    cnoremap' :: V.EventString -> EditorM () -> V.VimBinding
    cnoremap' key x = mkStringBindingE V.Ex V.Finish (key, x, id)
