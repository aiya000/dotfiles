-- Link to ~/.xmonad/xmonad.hs
-- - Target platforms:
--   - arch
-- - This config depends:
--   - arch:
--     - xmonad
--     - xmonad-contrib
--     - xmonad-extras-darcs

import Control.Monad ((>=>))
import Text.Printf (printf)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (UrgencyHook(..), withUrgencyHook)
import XMonad.Layout.Gaps (GapMessage(..))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, findTag)
import XMonad.Util.Dzen (DzenConfig, dzenConfig, onCurr, center, addArgs)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce (spawnOnce)


main :: IO ()
main = (xmobar >=> xmonad) $ desktopConfig
  { terminal    = "xterm"
  , modMask     = superMask
  , borderWidth = 2
  , layoutHook  = myLayoutHook
  , startupHook = myStartupHook
  , manageHook  = myManageHook
  , workspaces  = myWorkspaces
  }
  `additionalKeys` myKeymappings


-- The functions and the values
firstTerminal :: String
firstTerminal = "xfce4-terminal"

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

espeak :: String -> X ()
espeak msg = spawn $ "espeak -s 150 -v +fex \"" ++ msg ++ "\""

notifySend :: String -> String -> X ()
notifySend title msg = spawn $ printf "notify-send '%s' '%s'" title msg


-- My configurations

myLayoutHook = avoidStruts $ simpleTabbed ||| marginForXmobar ||| layoutHook desktopConfig
  where
    marginForXmobar = Tall 1 (3/100) (1/2)


myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawnOnce "xfce4-terminal -e tmux"
  setWMName "LG3D"  -- For Java Swing apps


myManageHook :: ManageHook
myManageHook = placeHook (fixed (0.5, 0.5)) <+> manageFloat <+> manageHook desktopConfig
  where
    manageFloat = composeAll
      [ className =? "Gimp" --> doFloat
      ]


myWorkspaces :: [String]
myWorkspaces = map show [1..4]


type KeyComb = (KeyMask, KeySym)
myKeymappings :: [(KeyComb, X ())]
myKeymappings =
  [ ((altMask, xK_l), cycleWindowsForward)
  , ((altMask, xK_h), cycleWindowsBackward)
  , ((altMask .|. shiftMask, xK_l), swapNextWindow)
  , ((altMask .|. shiftMask, xK_h), swapPrevWindow)
  , ((altMask, xK_Tab), nextScreen)
  , ((altMask, xK_F4), kill)
  , ((superMask, xK_F6), toggleMute    >> return ())
  , ((superMask, xK_F7), lowerVolume 5 >> return ())
  , ((superMask, xK_F8), raiseVolume 5 >> return ())
  -- Applications
  , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_r), spawn "dmenu_run")
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((noModMask, xK_Print), takeScreenShotFull)
  , ((shiftMask, xK_Print), takeScreenShotWindow)
  ]
  where
    cycleWindowsForward  = windows focusDown
    cycleWindowsBackward = windows focusUp
    swapNextWindow       = windows swapDown
    swapPrevWindow       = windows swapUp
    takeScreenShotFull   = do
      spawn "import -window root ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
      espeak "shot the area"
      notifySend "ImageMagick" "shot the area"
    takeScreenShotWindow = do
      spawn "import -window $(xdotool getwindowfocus -f) ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
      espeak "shot the window"
      notifySend "ImageMagick" "shot the window"
