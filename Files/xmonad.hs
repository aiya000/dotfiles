-- Link to ~/.xmonad/xmonad.hs

import Control.Monad ((>=>))
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Layout.Gaps (GapMessage(..))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)


main :: IO ()
main = (xmobar >=> xmonad) $ desktopConfig
  { terminal    = "xterm"
  , modMask     = superMask
  , borderWidth = 2
  , layoutHook  = simpleTabbed ||| layoutHook desktopConfig
  }
  `additionalKeys` myKeymappings


firstTerminal :: String
firstTerminal = "xfce4-terminal"


myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawnOnce firstTerminal


type KeyComb = (KeyMask, KeySym)

altMask :: KeyMask
altMask = mod1Mask
superMask :: KeyMask
superMask = mod4Mask
noMask :: KeyMask
noMask = 0

myKeymappings :: [(KeyComb, X ())]
myKeymappings =
  [ ((altMask, xK_l), cycleWindowsForward)
  , ((altMask, xK_h), cycleWindowsBackward)
  , ((altMask .|. shiftMask, xK_l), swapNextWindow)
  , ((altMask .|. shiftMask, xK_h), swapPrevWindow)
  , ((altMask, xK_Tab), nextScreen)
  --, ((altMask, xK_4), )
  --, ((superMask, xK_f), sendMessage (Toggle FULL))
  --, ((superMask, xK_g), sendMessage ToggleGaps)
  --, ((superMask, xK_j), sendMessage MirrorShrink)
  --, ((superMask, xK_k), sendMessage MirrorExpand)
  -- Applications
  , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_r), spawn "xfce4-appfinder --collapsed")
  , ((noMask, xK_Print), spawn "xfce4-screenshooter --fullscreen")
  , ((shiftMask, xK_Print), spawn "xfce4-screenshooter --window")
  ]
  where
    cycleWindowsForward  = windows focusDown
    cycleWindowsBackward = windows focusUp
    swapNextWindow       = windows swapDown
    swapPrevWindow       = windows swapUp
