import Control.Monad ((>=>), void)
import Control.Monad.Extra (ifM)
import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.FloatKeys (keysMoveWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Volume (toggleMute, lowerVolume, raiseVolume)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout (ChangeLayout(FirstLayout,NextLayout))
import XMonad.Layout.Gaps (gaps, Direction2D(U))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.SubLayouts (subTabbed, GroupMsg(MergeAll,UnMerge))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Operations (sendMessage, withFocused, mouseResizeWindow)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView, shift)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonadConfig.CommandWrapper (takeScreenShot)
import XMonadConfig.Shelly (switchKeyModeTo, currentKeyModeIs)
import qualified XMonadConfig.CommandWrapper as CW
import qualified XMonadConfig.Shelly as SH


main :: IO ()
main = do
  inHhkbMode <- currentKeyModeIs SH.HHKB
  let (myModMask, myKeys) = if inHhkbMode then (hhkbCasualMask, myHHKBKeys)
                                          else (superMask, myNormalKeys)
  (xmobar >=> xmonad) $ desktopConfig
    { terminal           = "termite"
    , modMask            = myModMask
    , borderWidth        = 2
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#0000ff"
    }
    `additionalKeys` myKeys
    `additionalMouseBindings` myMouseBindings


altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

hhkbCasualMask :: KeyMask
hhkbCasualMask = controlMask .|. shiftMask


myLayoutHook = twoTabbedPane ||| Grid
  where
    twoTabbedPane = subTabbed $ TwoPane (1/55) (1/2)

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "termite -e tmux"
  spawnOnce "tmux new-window 'nvim +\"cd ~/.dotfiles\" +terminal'"
  setWMName "LG3D"  -- For Java Swing apps starting

myManageHook :: ManageHook
myManageHook = placeHook (fixed (0.5, 0.5)) <+> manageFloatForTargets <+> manageHook desktopConfig
  where
    manageFloatForTargets = composeAll
      [ --className =? "Gimp" --> doFloat
      ]

myWorkspaces :: [String]
myWorkspaces = map show [1..4]


myNormalKeys :: [((KeyMask, KeySym), X ())]
myNormalKeys =
  [ ((altMask .|. controlMask, xK_t), spawn "termite")
  , ((altMask .|. shiftMask, xK_h), windows swapUp)
  , ((altMask .|. shiftMask, xK_i), nextScreen)
  , ((altMask .|. shiftMask, xK_l), windows swapDown)
  , ((altMask, xK_h), windows focusUp)
  , ((altMask, xK_j), withFocused (sendMessage . MergeAll))
  , ((altMask, xK_k), withFocused (sendMessage . UnMerge))
  , ((altMask, xK_l), windows focusDown)
  , ((hhkbCasualMask, xK_x), switchKeyModeTo SH.HHKB)
  , ((noModMask, xK_Print), takeScreenShot CW.FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot CW.ActiveWindow)
  , ((superMask .|. shiftMask, xK_F1), spawn "xscreensaver-command -lock; sudo pm-hibernate") -- ^ must add pm-hibernate to sudoers without inputting password
  , ((superMask .|. shiftMask, xK_a), sinkAll)
  , ((superMask .|. shiftMask, xK_i), sendMessage NextLayout)
  , ((superMask, xK_F10), CW.lockScreen)
  , ((superMask, xK_F11), CW.lockScreenSuspend)
  , ((superMask, xK_F12), CW.lockScreenHibernate)
  , ((superMask, xK_F4), spawn "light -U 10")
  , ((superMask, xK_F5), spawn "light -A 10")
  , ((superMask, xK_F6), void $ toggleMute)
  , ((superMask, xK_F7), void $ lowerVolume 5)
  , ((superMask, xK_F8), void $ raiseVolume 5)
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_h), withFocused $ keysMoveWindow (-5,0))
  , ((superMask, xK_j), withFocused $ keysMoveWindow (0,5))
  , ((superMask, xK_k), withFocused $ keysMoveWindow (0,-5))
  , ((superMask, xK_l), withFocused $ keysMoveWindow (5,0))
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((superMask, xK_r), spawn "dmenu_run")
  ]

myHHKBKeys :: [((KeyMask, KeySym), X ())]
myHHKBKeys =
  [ ((hhkbCasualMask .|. altMask, xK_h), windows swapUp)
  , ((hhkbCasualMask .|. altMask, xK_l), windows swapDown)
  , ((hhkbCasualMask, xK_a), sinkAll)
  , ((hhkbCasualMask, xK_c), kill)
  , ((hhkbCasualMask, xK_e), spawn "thunar")
  , ((hhkbCasualMask, xK_f), spawn "firefox")
  , ((hhkbCasualMask, xK_g), sendMessage NextLayout)
  , ((hhkbCasualMask, xK_h), windows focusUp)
  , ((hhkbCasualMask, xK_i), nextScreen)
  , ((hhkbCasualMask, xK_j), withFocused (sendMessage . MergeAll))
  , ((hhkbCasualMask, xK_k), withFocused (sendMessage . UnMerge))
  , ((hhkbCasualMask, xK_l), windows focusDown)
  , ((hhkbCasualMask, xK_m), spawn "xfce4-mixer")
  , ((hhkbCasualMask, xK_r), spawn "dmenu_run")
  , ((hhkbCasualMask, xK_t), spawn "termite")
  , ((hhkbCasualMask, xK_x), switchKeyModeTo SH.Common)
  , ((noModMask, xK_Print), takeScreenShot CW.FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot CW.ActiveWindow)
  , ((superMask, xK_F10), CW.lockScreen)
  , ((superMask, xK_F11), CW.lockScreenSuspend)
  , ((superMask, xK_F12), CW.lockScreenHibernate)
  , ((superMask, xK_F1), spawn "light -U 10")
  , ((superMask, xK_F2), spawn "light -A 10")
  , ((superMask, xK_F3), void $ lowerVolume 5)
  , ((superMask, xK_F4), void $ raiseVolume 5)
  , ((superMask, xK_Print), takeScreenShot CW.FullScreen)
  ]
  -- alt + shift + [1-9] to move the current window to the target worskpace
  ++ [((altMask, numKey), windows . shift $ workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces ]
  ++ [ ((hhkbCasualMask, numKey), windows . greedyView $ workspace)
     | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces ]


myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask, button1), mouseResizeWindow)
  ]
