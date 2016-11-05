{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Data.Prototype (override)
import Lens.Micro.Platform ((.=))
import Prelude hiding (foldl)
import System.Console.CmdArgs (cmdArgs)
import Yi.Boot (reload)
import Yi.Buffer.Basic (Direction(Forward))
import Yi.Buffer.HighLevel (readCurrentWordB)
import Yi.Buffer.Misc (setVisibleSelection)
import Yi.Config (configUI, configWindowFill)
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Lens (defaultKmA, configUIA, startActionsA)
import Yi.Config.Simple.Types (ConfigM, runConfigM)
import Yi.Core (startEditor, refreshEditor)
import Yi.Dired (dired)
import Yi.Editor (EditorM, MonadEditor, withEditor, closeBufferAndWindowE, withCurrentBuffer, printMsg)
import Yi.Event (Event)
import Yi.File (viWrite, fwriteAllY, openNewFile)
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Emacs.KillRing (killLine)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, pureEval)
import Yi.Keymap.Vim.EventUtils (eventToEventString)
import Yi.Keymap.Vim.StateUtils (switchModeE, resetCountE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import Yi.MyConfig.CmdOptions (CommandLineOptions(CommandLineOptions,frontend,startOnLine,files),clOptions)
import Yi.MyConfig.Helper (VimEvaluator, keyC, quitEditorWithBufferCheck, closeWinOrQuitEditor, switchModeY)
import Yi.Rope (YiString, fromString, toString)
import Yi.Search (doSearch, SearchOption(IgnoreCase))
import Yi.Types (Action(YiA,EditorA))

import qualified Yi.Buffer.Misc as B
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V


-- tabspace num 
tabspaceNum :: Int
tabspaceNum = 2

-- Entry point
main :: IO ()
main = do
  --TODO: Implement --frontend and --startonline
  args   <- cmdArgs clOptions
  let openFileActions = intersperse (EditorA E.newTabE) $ map (YiA . openNewFile) (files args)
  config <- flip execStateT defaultConfig . runConfigM $ do
    startActionsA .= openFileActions
    myConfig
  startEditor config Nothing


myConfig :: ConfigM ()
myConfig = do
  configureVty
  configureHaskellMode
  configureMiscModes
  configureMyVim

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
  , nnoremapY (keyC 'l') (refreshEditor >> printMsg "refreshed")
  , nnoremapE' " h"  E.prevWinE  -- temporary
  , nnoremapE' " j"  E.nextWinE  -- temporary
  , nnoremapE' " k"  E.prevWinE  -- temporary
  , nnoremapE' " l"  E.nextWinE  -- temporary
  , nnoremapE' "gH"  E.newTabE
  , nnoremapE' "ghh" E.newTabE  -- temporary
  , nnoremapY' "ghq" closeWinOrQuitEditor
  , nnoremapY' "ghQ" quitEditorWithBufferCheck
  , nnoremapE' "ghc" E.closeBufferAndWindowE
  , nnoremapE' "ghv" vsplit
  --, nnoremapE' "ghs" (E.splitE >> ?)  -- Clone win to under
  , nnoremapE' "gho" E.closeOtherE  -- Do :only
  --, nnoremapE' "g:"  (eval ":buffers<CR>")  --FIXME: doesn't works
  , nnoremapE' "gh\"" (resizeCurrentWin 3)
  , nnoremapY' "<C-k><C-r>" reload
  , nnoremapY' "<C-k><CR>"  viWrite  -- Yi interprets <C-j> as <CR>
  , nnoremapY' "<C-k>J"     (fwriteAllY >> return ())
  , nnoremapY' "\\e"        (withEditor vsplit    >> dired)
  , nnoremapY' "\\\\E"      (withEditor E.newTabE >> dired)
  --, nnoremapE' "<C-k><C-l>" (eval ":nohlsearch<CR>")  --FIXME: doesn't works correctly
  , nnoremapY' "<CR>" (withCurrentBuffer $ B.lineDown >> B.newlineB >> B.lineUp)  -- insert newline to under
  -- Override default
  --TODO: implement
  --, nnoremapE' ">>" tabspaceNum
  --, nnoremapE' "<<" tabspaceNum
  -- Complete official lost things
  , nnoremapE' "<C-w>w" E.nextWinE
  , nnoremapE' "gd"     searchFromHead
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
    -- Clone a window to right, this means default behavior of Vim :vsplit
    vsplit = E.splitE >> E.prevWinE
    -- normal gd of Vim
    searchFromHead = do
      word <- toString <$> withCurrentBuffer readCurrentWordB
      withCurrentBuffer $  B.moveToLineColB 0 0
      doSearch (Just word) [IgnoreCase] Forward
      return ()


-- Keymappings for V.VimMode (∀a. V.Insert a)
insertBindings :: [V.VimBinding]
insertBindings =
  [ inoremapE (keyC 'l') (switchModeE V.Normal >> withCurrentBuffer B.leftB)  -- <Esc> behavior of Vim
  --FIXME: cannot unset modified flag
  , inoremapY' "<C-k><CR>" (viWrite >> switchModeY V.Normal)  -- Yi interprets <C-j> as <CR>
  , inoremapY' "<C-k><C-k>" (killLine Nothing)
  -- Override default
  , inoremapY' "<Tab>" (withCurrentBuffer $ B.insertN . fromString $ replicate tabspaceNum ' ')  --NOTE: Does Yi has :set ts=n like stateful function ?
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
