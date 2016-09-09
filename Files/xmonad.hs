-- Link to ~/.xmonad/xmonad.hs
-- - Target platforms:
--   - arch
-- - This config depends:
--   - arch:
--     - xmonad
--     - xmonad-contrib
--     - xmonad-extras-darcs

import Control.Monad ((>=>))
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Layout.Gaps (GapMessage(..))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Util.Dzen (DzenConfig, dzenConfig, onCurr, center, addArgs)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)

--TODO:
-- - Alt+4 kill the active app and the window
-- - split the window in one screen


main :: IO ()
main = (xmobar >=> xmonad) $ desktopConfig
  { terminal    = "xterm"
  , modMask     = superMask
  , borderWidth = 2
  , layoutHook  = simpleTabbed ||| layoutHook desktopConfig
  , startupHook = myStartupHook
  , workspaces  = myWorkspaces
  }
  `additionalKeys` myKeymappings


-- Variables
firstTerminal :: String
firstTerminal = "xfce4-terminal"

altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask


-- My configurations

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawnOnce firstTerminal


myWorkspaces :: [String]
myWorkspaces = ["1:main"] ++ map show [2..4]


type KeyComb = (KeyMask, KeySym)
myKeymappings :: [(KeyComb, X ())]
myKeymappings =
  [ ((altMask, xK_l), cycleWindowsForward)
  , ((altMask, xK_h), cycleWindowsBackward)
  , ((altMask .|. shiftMask, xK_l), swapNextWindow)
  , ((altMask .|. shiftMask, xK_h), swapPrevWindow)
  , ((altMask, xK_Tab), nextScreen)
  , ((altMask, xK_F4), kill)
  --, ((superMask, xK_f), sendMessage (Toggle FULL))
  --, ((superMask, xK_g), sendMessage ToggleGaps)
  --, ((superMask, xK_j), sendMessage MirrorShrink)
  --, ((superMask, xK_k), sendMessage MirrorExpand)
  , ((superMask, xK_F6), toggleMute     >> return ())
  , ((superMask, xK_F7), lowerVolume 10 >> return ())
  , ((superMask, xK_F8), raiseVolume 10 >> return ())
  -- Applications
  , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_r), spawn "xfce4-appfinder --collapsed")
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((noModMask, xK_Print), spawn "xfce4-screenshooter --fullscreen")
  , ((shiftMask, xK_Print), spawn "xfce4-screenshooter --window")  --FIXME: don't take the active window, but took full screen
  ]
  where
    cycleWindowsForward  = windows focusDown
    cycleWindowsBackward = windows focusUp
    swapNextWindow       = windows swapDown
    swapPrevWindow       = windows swapUp
