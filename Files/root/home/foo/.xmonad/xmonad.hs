-- Link to ~/.xmonad/xmonad.hs
-- - Target platforms:
--   - arch
-- - This config depends:
--   - arch:
--     - xmonad
--     - xmonad-contrib
--     - xmonad-extras-darcs

-- imports -- {{{

import Control.Monad ((>=>))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import System.Directory (doesFileExist)
import Text.Printf (printf)
import XMonad
import XMonad.Actions.CycleWS (prevScreen, nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Actions.Workscreen (shiftToWorkscreen)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (ChangeLayout(FirstLayout,NextLayout))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.SubLayouts (subTabbed, onGroup, GroupMsg(MergeAll,UnMerge))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Operations (sendMessage, withFocused)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)

-- }}}


main :: IO ()
main = (xmobar >=> xmonad) $ desktopConfig
  { terminal           = "xterm"
  , modMask            = superMask
  , borderWidth        = 2
  , layoutHook         = myLayoutHook
  , startupHook        = myStartupHook
  , manageHook         = myManageHook
  , workspaces         = myWorkspaces
  , focusedBorderColor = "#0000ff"
  }
  `additionalKeys` myKeymappings


-- Data Types {{{

type KeyComb = (KeyMask, KeySym)

data ScreenShotType = FullScreen | ActiveWindow deriving (Eq)

-- }}}
-- Functions and Values {{{

firstTerminal :: String
firstTerminal = "xfce4-terminal -e tmux"

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

-- This depends ImageMagick and xdotool
screenshot :: ScreenShotType -> FilePath -> X ()
screenshot FullScreen   path = spawn $ printf "import -window root %s" path
screenshot ActiveWindow path = spawn $ printf "import -window $(xdotool getwindowfocus -f) %s" path

espeak :: String -> X ()
espeak msg = spawn $ "espeak -s 150 -v +fex \"" ++ msg ++ "\""

notifySend :: String -> String -> X ()
notifySend title msg = spawn $ printf "notify-send '%s' '%s'" title msg

-- }}}
-- My configurations {{{

myLayoutHook = subTabbed $ Grid ||| TwoPane (1/55) (1/2)

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawn firstTerminal
  setWMName "LG3D"  -- For Java Swing apps starting


myManageHook :: ManageHook
myManageHook = placeHook (fixed (0.5, 0.5)) <+> manageFloatForTargets <+> manageHook desktopConfig
  where
    manageFloatForTargets = composeAll
      [ --className =? "Gimp" --> doFloat
      ]


-- myWorkspaces must be made by myWorkspaces'
myWorkspaces' :: [Int]
myWorkspaces' = [1 .. 4]
myWorkspaces :: [String]
myWorkspaces = map show myWorkspaces'


myKeymappings :: [(KeyComb, X ())]
myKeymappings =
  -- movements Just for myWorkspaces
  let numKeys            = [xK_1 .. xK_9] ++ [xK_0]
      workspaceNum       = length myWorkspaces'
      makeMovement key n = ((altMask .|. shiftMask, key), moveWindowTo n)
      movements          = zipWith makeMovement numKeys $ map S myWorkspaces'
  in [ ((altMask, xK_h), cycleWindowsBackward)
     , ((altMask, xK_l), cycleWindowsForward)
     , ((altMask, xK_j), withFocused (sendMessage . MergeAll))
     , ((altMask, xK_k), withFocused (sendMessage . UnMerge))
     --NOTE: add keymap keysMoveWindow and swap(Next|Prev)Window to some keys with branching by current layout state
     --((altMask, xK_l), withFocused $ keysMoveWindow (2,0))
     --, ((altMask, xK_h), withFocused $ keysMoveWindow (-2,0))
     --, ((altMask, xK_j), withFocused $ keysMoveWindow (0,2))
     --, ((altMask, xK_k), withFocused $ keysMoveWindow (0,-2))
     , ((altMask, xK_Tab), nextScreen)
     , ((altMask .|. shiftMask, xK_i), nextScreen)
     , ((superMask, xK_l), swapNextWindow)
     , ((superMask, xK_h), swapPrevWindow)
     , ((superMask, xK_F6), toggleMute    >> return ())
     , ((superMask, xK_F7), lowerVolume 5 >> return ())
     , ((superMask, xK_F8), raiseVolume 5 >> return ())
     , ((superMask .|. shiftMask, xK_h), sendMessage FirstLayout)
     , ((superMask .|. shiftMask, xK_l), sendMessage NextLayout)
     , ((superMask .|. shiftMask, xK_a), sinkAll)
     -- Applications
     , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
     , ((superMask, xK_e), spawn "thunar")
     , ((superMask, xK_r), spawn "dmenu_run")
     , ((superMask, xK_l), spawn "xscreensaver-command -lock")
     , ((superMask, xK_f), spawnOnce "firefox")
     , ((superMask, xK_m), spawnOnce "xfce4-mixer")
     , ((noModMask, xK_Print), takeScreenShot FullScreen)
     , ((shiftMask, xK_Print), takeScreenShot ActiveWindow)
     ] ++ movements
  where
    cycleWindowsForward  = windows focusDown
    cycleWindowsBackward = windows focusUp
    swapNextWindow       = windows swapDown
    swapPrevWindow       = windows swapUp

    takeScreenShot :: ScreenShotType -> X ()
    takeScreenShot ssType = do
      let msg = messageOf ssType
      screenshot ssType dateSSPath
      notifySend "ScreenShot" msg
      espeak msg
      where
        dateSSPath             = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
        messageOf FullScreen   = "shot the full screen"
        messageOf ActiveWindow = "shot the active window"

    moveWindowTo :: ScreenId -> X ()
    moveWindowTo (S n) = let workscreenId = (n - 1) `mod` length myWorkspaces'
                         in shiftToWorkscreen workscreenId

-- }}}
