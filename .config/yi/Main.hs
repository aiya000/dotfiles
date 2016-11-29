{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Control.Monad.State.Lazy (execStateT)
import Data.List (intersperse)
import Data.Prototype (override)
import Lens.Micro.Platform ((.=))
import Prelude hiding (foldl)
import System.Console.CmdArgs (cmdArgs)
import Yi.Buffer.Basic (Direction(Forward))
import Yi.Buffer.HighLevel (readCurrentWordB)
import Yi.Buffer.Misc (setVisibleSelection)
import Yi.Config (configUI, configWindowFill)
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Pango (configurePango)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Lens (defaultKmA, configUIA, startActionsA, initialActionsA)
import Yi.Config.Simple.Types (ConfigM, runConfigM)
import Yi.Core (startEditor, refreshEditor)
import Yi.Dired (dired)
import Yi.Editor (EditorM, MonadEditor, withEditor, closeBufferAndWindowE, withCurrentBuffer)
import Yi.File (viWrite, fwriteAllY, openNewFile)
import Yi.Keymap (YiM, KeymapSet)
import Yi.Keymap.Emacs.KillRing (killLine)
import Yi.Keymap.Vim (mkKeymapSet, defVimConfig, vimBindings, pureEval)
import Yi.Keymap.Vim.StateUtils (switchModeE, resetCountE, modifyStateE)
import Yi.Keymap.Vim.Utils (mkStringBindingE, mkStringBindingY)
import Yi.Keymap.Vim.Ex.Commands.Registers (printRegisters)
import Yi.MyConfig.CmdOptions
import Yi.MyConfig.Helper
import Yi.Rope (fromString, toString)
import Yi.Search (doSearch, SearchOption(IgnoreCase))
import Yi.Tag (TagTable, setTags)
import Yi.Types (Action(YiA,EditorA))
import qualified Yi.Buffer.HighLevel as BH
import qualified Yi.Buffer.Misc as B
import qualified Yi.Editor as E
import qualified Yi.Keymap.Vim.Common as V

-- For debug
import Yi.Debug (initDebug, logPutStrLn)
import Yi.Editor (printMsg)
import Yi.String (showT)
import Data.Monoid ((<>))


-- tabspace num 
tabspaceNum :: Int
tabspaceNum = 2

-- Entry point
main :: IO ()
main = do
  initDebug "/home/aiya000/.tmp/yi.log"
  --TODO: Implement --startonline
  args        <- cmdArgs clOptions
  mayTagTable <- findTagTable
  let openFileActions = intersperse (EditorA E.newTabE) $ map (YiA . openNewFile) (files args)
  config      <- flip execStateT defaultConfig . runConfigM $ myConfig args mayTagTable >> startActionsA .= openFileActions
  startEditor config Nothing


myConfig :: CommandLineOptions -> Maybe TagTable -> ConfigM ()
myConfig clo mayTagTable = do
  case frontend clo of
    "vty"   -> configureVty
    "pango" -> configurePango
    x       -> error $ "unknown frontend: " ++ x
  case mayTagTable of
    Nothing       -> return ()
    Just tagTable -> do
      let actions = [ EditorA $ setTags tagTable ]
      initialActionsA .= actions
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
  [ nnoremapE "<C-p>" E.previousTabE
  , nnoremapE "<C-n>" E.nextTabE
  , nnoremapY "<C-l>" refreshEditor
  , nnoremapE " h"    E.prevWinE  -- temporary
  , nnoremapE " j"    E.nextWinE  -- temporary
  , nnoremapE " k"    E.prevWinE  -- temporary
  , nnoremapE " l"    E.nextWinE  -- temporary
  , nnoremapE "q:"    printRegisters
  --, nnoremapE "g:"  (eval ":buffers<CR>")  --FIXME: doesn't works
  , nnoremapE "g*"  staySearch
  , nnoremapE "gH"  E.newTabE
  , nnoremapE "ghh" E.newTabE  -- temporary
  , nnoremapY "ghq" closeWinOrQuitEditor
  , nnoremapY "ghQ" quitEditorIfModifiedNothing
  , nnoremapE "ghc" E.closeBufferAndWindowE
  , nnoremapE "ghv" vsplit
  --, nnoremapE "ghs" (E.splitE >> ?)  -- Clone win to under
  , nnoremapE "gho"        E.closeOtherE  -- Do :only
  , nnoremapE "gh\""       (resizeCurrentWin 3)
  , nnoremapY "<C-k><CR>"  viWrite  -- Vty-Yi interprets <C-j> as <CR>
  , nnoremapY "<C-k>J"     (void fwriteAllY)
  , nnoremapY "\\e"        (withEditor vsplit    >> dired)
  , nnoremapY "\\\\E"      (withEditor E.newTabE >> dired)
  --, nnoremapE "<C-k><C-l>" (eval ":nohlsearch<CR>")  --FIXME: doesn't works correctly
  , nnoremapE "<CR>" (withCurrentBuffer $ BH.moveToEol >> B.newlineB)
  -- Override default
  --TODO: implement
  --, nnoremapE ">>" tabspaceNum
  --, nnoremapE "<<" tabspaceNum
  -- Complete official lost things
  , nnoremapE "<C-w>w" E.nextWinE
  , nnoremapE "gd"     searchFromHead
  --TODO: implement suspend
  --, nnoremapY "<C-z>"
  ]
  where
    -- Like nnoremap of Vim for EditorM
    nnoremapE :: V.EventString -> EditorM () -> V.VimBinding
    nnoremapE key x = mkStringBindingE V.Normal V.Drop (key, x, id)
    -- for YiM
    nnoremapY :: V.EventString -> YiM () -> V.VimBinding
    nnoremapY key x = mkStringBindingY V.Normal (key, x, id)

    resizeCurrentWin :: Int -> EditorM ()
    resizeCurrentWin lineNum = undefined
    -- normal gd of Vim
    searchFromHead :: EditorM ()
    searchFromHead = do
      word <- toString <$> withCurrentBuffer readCurrentWordB
      withCurrentBuffer $  B.moveToLineColB 0 0
      void $ doSearch (Just word) [IgnoreCase] Forward
    -- Highlight word
    staySearch = do
      (x,y) <- withCurrentBuffer BH.getLineAndCol
      word <- toString <$> withCurrentBuffer readCurrentWordB
      void $ doSearch (Just word) [IgnoreCase] Forward
      withCurrentBuffer $ B.moveToLineColB x y
    -- Clone a window to right, this means default behavior of Vim's :vsplit
    vsplit = E.splitE >> E.prevWinE


-- Keymappings for V.VimMode (∀a. V.Insert a)
insertBindings :: [V.VimBinding]
insertBindings =
  --FIXME: yi has gone when block inserted
  [ inoremapE "<C-l>"      (exitInsert >> withCurrentBuffer B.leftB) -- <Esc> behavior of Vim
  --FIXME: cannot unset modified flag
  , inoremapY "<C-k><CR>"  (withEditor exitInsert >> viWrite)  -- Vty-Yi interprets <C-j> as <CR>
  , inoremapY "<C-k><C-k>" (killLine Nothing)
  -- Override default
  , inoremapY "<Tab>" (withCurrentBuffer $ B.insertN . fromString $ replicate tabspaceNum ' ')  --NOTE: Does Yi has :set ts=n like stateful function ?
  -- Complete official lost things
  , inoremapY "<C-o>O" $ withCurrentBuffer $
      ifM ((==1) . fst <$> BH.getLineAndCol)
        (BH.moveToSol >> B.newlineB >> B.lineUp)
        (B.lineUp >> BH.moveToEol >> B.newlineB)
  , inoremapY "<C-o>o" (withCurrentBuffer $ BH.moveToEol >> B.newlineB)
  , inoremapY "<C-o>I" (withCurrentBuffer BH.firstNonSpaceB)
  , inoremapY "<C-o>A" (withCurrentBuffer $ BH.lastNonSpaceB >> B.rightB)
  , inoremapY "<C-o>h" (withCurrentBuffer B.leftB)
  , inoremapY "<C-o>l" (withCurrentBuffer B.rightB)
  ]
  where
    -- The keymapping implementor for both of V.VimBindingY and V.VimBindingE
    implBinding :: MonadEditor m => V.EventString -> m () -> V.EventString -> V.VimState -> V.MatchResult (m V.RepeatToken)
    implBinding key context = \key' state ->
      case V.vsMode state of
        V.Insert _ -> (const $ context >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch
    -- Like inoremap of Vim for EditorM
    -- for ∀a. V.Insert a
    inoremapE :: V.EventString -> EditorM () -> V.VimBinding
    inoremapE key x = V.VimBindingE $ implBinding key x
    -- Like inoremap of Vim for YiM
    -- for ∀a. V.Insert a
    inoremapY :: V.EventString -> YiM () -> V.VimBinding
    inoremapY key x = V.VimBindingY $ implBinding key x

    -- See https://www.stackage.org/haddock/lts-7.4/yi-0.12.6/src/Yi.Keymap.Vim.InsertMap.html#exitBinding
    exitInsert :: EditorM ()
    exitInsert = do
      modifyStateE $ \s -> s
        { V.vsOngoingInsertEvents = mempty
        , V.vsSecondaryCursors    = mempty
        }
      switchModeE V.Normal


-- Keymapping for V.VimMode (∀a V.Visual a)
visualBindings :: [V.VimBinding]
visualBindings =
  [ vnoremapE "<C-l>" exitVisual
  -- Complete official lost things
  --TODO: , vnoremapE "p" 
  ]
  where
    -- The keymappings implementor for both of V.VimBindingY and V.VimBindingE
    implBinding :: MonadEditor m => V.EventString -> m () -> V.EventString -> V.VimState -> V.MatchResult (m V.RepeatToken)
    implBinding key context = \key' state ->
      case V.vsMode state of
        V.Visual _ -> (const $ context >> return V.Continue) <$> key' `V.matchesString` key
        _          -> V.NoMatch
    -- Like vnoremap of Vim for EditorM
    vnoremapE :: V.EventString -> EditorM () -> V.VimBinding
    vnoremapE key x = V.VimBindingE $ implBinding key x

    -- See https://www.stackage.org/haddock/lts-7.4/yi-0.12.6/src/Yi.Keymap.Vim.ExMap.html#exitEx
    exitVisual :: EditorM ()
    exitVisual = do
      resetCountE
      switchModeE V.Normal
      withCurrentBuffer $ setVisibleSelection False

-- Keymappings for V.VimMode V.Ex
exBindings :: [V.VimBinding]
exBindings =
  [ cnoremapE "<C-l>" exitEx'
  ]
  where
    -- Like cnoremap of Vim for EditorM ()
    cnoremapE :: V.EventString -> EditorM () -> V.VimBinding
    cnoremapE key x = mkStringBindingE V.Ex V.Finish (key, x, id)

    -- See https://www.stackage.org/haddock/lts-7.4/yi-0.12.6/src/Yi.Keymap.Vim.ExMap.html#exitEx
    exitEx' :: EditorM ()
    exitEx' = do
      resetCountE
      switchModeE V.Normal
      closeBufferAndWindowE
      withCurrentBuffer $ setVisibleSelection False
