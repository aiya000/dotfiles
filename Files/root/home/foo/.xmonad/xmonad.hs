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
main = (xmobar >=> xmonad) $ withUrgencyHook LibNotifyHook $ desktopConfig
  { terminal    = "xterm"
  , modMask     = superMask
  , borderWidth = 2
  , layoutHook  = avoidStruts $ simpleTabbed ||| layoutHook desktopConfig
  , startupHook = myStartupHook
  , workspaces  = myWorkspaces
  }
  `additionalKeys` myKeymappings


-- Data types

-- See https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyHook = LibNotifyHook deriving (Read, Show)
instance UrgencyHook LibNotifyHook where
  urgencyHook LibNotifyHook w = do
    name       <- getName w
    maybeIndex <- findTag w <$> gets windowset
    case maybeIndex of
      Nothing    -> notifySend [">> Fatal error on the urgencyHook"]
      Just index -> notifySend [show name, "workspace '" ++ index ++ "'"]


-- The functions and the values
firstTerminal :: String
firstTerminal = "xfce4-terminal"

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

espeak :: String -> X ()
espeak msg = spawn $ "espeak -s 150 -v +fex \"" ++ msg ++ "\""

notifySend :: [String] -> X ()
notifySend xs = spawn $ "notify-send " ++ foldr1 catWithStyles xs
  where
    catWithStyles = printf "'%s' '%s'"


-- My configurations

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawnOnce "xfce4-terminal -e tmux"


myWorkspaces :: [String]
myWorkspaces = "1:main" : map show [2..4]


type KeyComb = (KeyMask, KeySym)
myKeymappings :: [(KeyComb, X ())]
myKeymappings =
  [ ((altMask, xK_l), cycleWindowsForward)
  , ((altMask, xK_h), cycleWindowsBackward)
  , ((altMask .|. shiftMask, xK_l), swapNextWindow)
  , ((altMask .|. shiftMask, xK_h), swapPrevWindow)
  , ((altMask, xK_Tab), nextScreen)
  , ((altMask, xK_F4), kill)
  , ((superMask, xK_F6), toggleMute     >> return ())
  , ((superMask, xK_F7), lowerVolume 10 >> return ())
  , ((superMask, xK_F8), raiseVolume 10 >> return ())
  -- Applications
  , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_r), spawn "dmenu_run")
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((noModMask, xK_Print), takeScreenShotFull)  --TODO: notify to xmobar
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
    takeScreenShotWindow = do
      spawn "import -window $(xdotool getwindowfocus -f) ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
      espeak "shot the window"
