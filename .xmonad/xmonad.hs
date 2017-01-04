import Control.Monad ((>=>), void)
import Control.Monad.Extra (ifM)
import System.EasyFile (doesFileExist, removeFile)
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
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown, greedyView)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonadConfig.Actions (moveWindowTo)
import XMonadConfig.CommandWrapper (takeScreenShot, touch, xmonadRestartWithMessage)
import XMonadConfig.Types (ScreenShotType (FullScreen, ActiveWindow))


hhkbKeyModeFlagFile :: FilePath
hhkbKeyModeFlagFile = "/tmp/xmonad-keymode-hhkb"

main :: IO ()
main = do
  myKeys <- ifM (doesFileExist hhkbKeyModeFlagFile)
              (return myHHKBKeys)
              (return myNormalKeys)
  (xmobar >=> xmonad) $ desktopConfig
    { terminal           = "termite"
    , modMask            = superMask
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


myLayoutHook = xmobarMargin . subTabbed $ TwoPane (1/55) (1/2) ||| Grid
  where
    xmobarMargin = gaps [(U, 13)]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "termite -e tmux"
  setWMName "LG3D"  -- For Java Swing apps starting

myManageHook :: ManageHook
myManageHook = placeHook (fixed (0.5, 0.5)) <+> manageFloatForTargets <+> manageHook desktopConfig
  where
    manageFloatForTargets = composeAll
      [ --className =? "Gimp" --> doFloat
      ]

myWorkspaces :: [String]
myWorkspaces = map show [1..4]


baseKeys :: KeyMask -> [((KeyMask, KeySym), X ())]
baseKeys casualMask =
  [ ((casualMask, xK_h), windows focusUp)
  , ((casualMask, xK_l), windows focusDown)
  , ((casualMask, xK_j), withFocused (sendMessage . MergeAll))
  , ((casualMask, xK_k), withFocused (sendMessage . UnMerge))
  , ((superMask, xK_l), withFocused $ keysMoveWindow (5,0))
  , ((superMask, xK_h), withFocused $ keysMoveWindow (-5,0))
  , ((superMask, xK_j), withFocused $ keysMoveWindow (0,5))
  , ((superMask, xK_k), withFocused $ keysMoveWindow (0,-5))
  , ((superMask .|. shiftMask, xK_i), sendMessage NextLayout)
  , ((superMask, xK_F1),  spawn "xscreensaver-command -lock; sudo pm-suspend") -- ^ must add pm-suspend to sudoers without inputting password
  , ((superMask, xK_F4),  spawn "light -U 10")
  , ((superMask, xK_F5),  spawn "light -A 10")
  , ((superMask, xK_F6),  void $ toggleMute)
  , ((superMask, xK_F7),  void $ lowerVolume 5)
  , ((superMask, xK_F8),  void $ raiseVolume 5)
  , ((superMask, xK_F12), spawn "xscreensaver-command -lock")
  , ((superMask .|. shiftMask, xK_F1), spawn "xscreensaver-command -lock; sudo pm-hibernate") -- ^ must add pm-hibernate to sudoers without inputting password
  ]

myNormalKeys :: [((KeyMask, KeySym), X ())]
myNormalKeys =
  [ ((altMask .|. shiftMask, xK_l), windows swapDown)
  , ((altMask .|. shiftMask, xK_h), windows swapUp)
  , ((altMask .|. shiftMask, xK_i), nextScreen)
  , ((superMask .|. shiftMask, xK_a), sinkAll)
  , ((altMask .|. controlMask, xK_t), spawn "termite")
  , ((superMask, xK_e), spawn "thunar")
  , ((superMask, xK_r), spawn "dmenu_run")
  , ((superMask, xK_f), spawn "firefox")
  , ((superMask, xK_m), spawn "xfce4-mixer")
  , ((noModMask, xK_Print), takeScreenShot FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot ActiveWindow)
  , ((hhkbCasualMask, xK_x), xmonadSwitchKeyModeToHHKB)
  ]
  ++ [ ((altMask .|. shiftMask, numKey), moveWindowTo myWorkspaces workspace)
      | (numKey, workspace) <- zip [xK_1 .. xK_9] . map S $ [1 .. length myWorkspaces] ]
  ++ baseKeys altMask
  where
    xmonadSwitchKeyModeToHHKB :: X ()
    xmonadSwitchKeyModeToHHKB = touch hhkbKeyModeFlagFile >> xmonadRestartWithMessage


myHHKBKeys :: [((KeyMask, KeySym), X ())]
myHHKBKeys =
  [ ((hhkbCasualMask .|. altMask, xK_l), windows swapDown)
  , ((hhkbCasualMask .|. altMask, xK_h), windows swapUp)
  , ((hhkbCasualMask, xK_i), nextScreen)
  , ((hhkbCasualMask, xK_c), kill)
  , ((hhkbCasualMask, xK_a), sinkAll)
  , ((hhkbCasualMask, xK_t), spawn "termite")
  , ((hhkbCasualMask, xK_e), spawn "thunar")
  , ((hhkbCasualMask, xK_r), spawn "dmenu_run")
  , ((hhkbCasualMask, xK_f), spawn "firefox")
  , ((hhkbCasualMask, xK_m), spawn "xfce4-mixer")
  , ((hhkbCasualMask, xK_x), xmonadSwitchKeyModeToNormal)
  , ((noModMask, xK_Print), takeScreenShot FullScreen)
  , ((shiftMask, xK_Print), takeScreenShot ActiveWindow)
  ]
  ++ [ ((hhkbCasualMask, numKey), moveWindowTo myWorkspaces workspace)
      | (numKey, workspace) <- zip [xK_1 .. xK_9] . map S $ [1 .. length myWorkspaces] ]
  ++ [ ((hhkbCasualMask, numKey), windows . greedyView $ workspace)
       | (numKey, workspace) <- zip [xK_1 .. xK_9] myWorkspaces ]
  ++ baseKeys hhkbCasualMask
  where
    xmonadSwitchKeyModeToNormal :: X ()
    xmonadSwitchKeyModeToNormal = (liftIO $ removeFile hhkbKeyModeFlagFile) >> xmonadRestartWithMessage


myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((altMask, button1), mouseResizeWindow)
  ]
