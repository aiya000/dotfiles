-- Link to ~/.xmonad/xmonad.hs
-- - Target platforms:
--   - arch
-- - This config depends:
--   - arch:
--     - xmonad
--     - xmonad-contrib
--     - xmonad-extras-darcs

-- imports -- {{{

import Control.Concurrent (threadDelay)
import Control.Monad ((>=>), void)
import Text.Printf (printf)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Actions.Workscreen (shiftToWorkscreen)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (ChangeLayout(FirstLayout,NextLayout))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.SubLayouts (subTabbed, GroupMsg(MergeAll,UnMerge))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Operations (sendMessage, withFocused, mouseResizeWindow)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.Gaps (gaps, Direction2D(U))

-- }}}


main :: IO ()
main = (xmobar >=> xmonad) $ desktopConfig
  { terminal           = "xfce4-terminal"
  , modMask            = superMask
  , borderWidth        = 2
  , layoutHook         = myLayoutHook
  , startupHook        = myStartupHook
  , manageHook         = myManageHook
  , workspaces         = myWorkspaces
  , focusedBorderColor = "#0000ff"
  }
  `additionalKeys` myKeys
  `additionalMouseBindings` myMouseBindings


-- Data Types {{{

data ScreenShotType = FullScreen | ActiveWindow deriving (Eq)

-- }}}
-- Functions and Values {{{

firstTerminal :: String
firstTerminal = "xfce4-terminal"

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

sleep :: Int -> X ()
sleep n | n < 0     = io $ error "argument must be over 0"
        | otherwise = io $ threadDelay (n * 1000000)

-- }}}
-- My configurations {{{

myLayoutHook = xmobarMargin . subTabbed $ TwoPane (1/55) (1/2) ||| Grid
  where
    xmobarMargin = gaps [(U, 13)]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "fcitx"
  spawnOnce "xfce4-clipman"
  spawnOnce "xfce4-terminal -e tmux"
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


myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  -- movements Just for myWorkspaces
  let numKeys            = [xK_1 .. xK_9] ++ [xK_0]
      workspaceNum       = length myWorkspaces'
      makeMovement key n = ((altMask .|. shiftMask, key), moveWindowTo n)
      movements          = zipWith makeMovement numKeys $ map S myWorkspaces'
  in [ ((altMask, xK_h), cycleWindowsBackward)
     , ((altMask, xK_l), cycleWindowsForward)
     , ((altMask, xK_j), withFocused (sendMessage . MergeAll))
     , ((altMask, xK_k), withFocused (sendMessage . UnMerge))
     , ((altMask .|. shiftMask, xK_l), swapNextWindow)
     , ((altMask .|. shiftMask, xK_h), swapPrevWindow)
     , ((altMask .|. shiftMask, xK_Tab), nextScreen)
     , ((altMask .|. shiftMask, xK_i),   nextScreen)
     , ((superMask, xK_l), withFocused $ keysMoveWindow (2,0))
     , ((superMask, xK_h), withFocused $ keysMoveWindow (-2,0))
     , ((superMask, xK_j), withFocused $ keysMoveWindow (0,2))
     , ((superMask, xK_k), withFocused $ keysMoveWindow (0,-2))
     , ((superMask .|. shiftMask, xK_h), sendMessage FirstLayout)
     , ((superMask .|. shiftMask, xK_l), sendMessage NextLayout)
     , ((superMask .|. shiftMask, xK_a), sinkAll)
     -- Hardware keys
     , ((superMask, xK_F1), spawn "sudo pm-suspend")
     , ((superMask, xK_F4), spawn "light -U 10")
     , ((superMask, xK_F5), spawn "light -A 10")
     , ((superMask, xK_F6), void $ toggleMute)
     , ((superMask, xK_F7), void $ lowerVolume 5)
     , ((superMask, xK_F8), void $ raiseVolume 5)
     -- Applications
     , ((altMask .|. controlMask, xK_t), spawn firstTerminal)
     , ((superMask, xK_e), spawn "thunar")
     , ((superMask, xK_r), spawn "dmenu_run")
     , ((superMask, xK_l), spawn "xscreensaver-command -lock")
     , ((superMask, xK_f), spawn "firefox")
     , ((superMask, xK_m), spawn "xfce4-mixer")
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
      espeak msg
      sleep 1
      notifySend "ScreenShot" msg
      where
        dateSSPath             = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
        messageOf FullScreen   = "shot the full screen"
        messageOf ActiveWindow = "shot the active window"

    moveWindowTo :: ScreenId -> X ()
    moveWindowTo (S n) = let workscreenId = (n - 1) `mod` length myWorkspaces'
                         in shiftToWorkscreen workscreenId


myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask, button1), mouseResizeWindow)
  ]

-- }}}
