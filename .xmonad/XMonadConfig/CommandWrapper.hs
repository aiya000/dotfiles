-- | The cli command wrappers of X
module XMonadConfig.CommandWrapper
  ( takeScreenShot
  , ScreenShotType (..)
  , lockScreen
  , lockScreenSuspend
  , lockScreenHibernate
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import XMonad.Core (X, spawn)

data ScreenShotType = FullScreen | ActiveWindow


-- |
-- ImageMagick wrapper with espeak, notify-send and xdotool.
-- Take screenshot as ScreenShotType to ~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png,
-- and notify finishing with espeak and notify-send
takeScreenShot :: ScreenShotType -> X ()
takeScreenShot ssType = do
  let msg = messageOf ssType
  screenshot ssType dateSSPath
  spawn  $ printf "espeak -s 150 -v +fex '%s'" msg
  liftIO $ threadDelay 1000000  -- Wait 1 sec
  spawn  $ printf "notify-send 'ScreenShot' '%s'" msg
  where
    screenshot :: ScreenShotType -> FilePath -> X ()
    screenshot FullScreen   path = spawn $ printf "import -window root %s" path
    screenshot ActiveWindow path = spawn $ printf "import -window $(xdotool getwindowfocus -f) %s" path

    dateSSPath             = "~/Picture/ScreenShot-$(date +'%Y-%m-%d-%H-%M-%S').png"
    messageOf FullScreen   = "shot the full screen"
    messageOf ActiveWindow = "shot the active window"

-- | `xscreensaver-command -lock`
lockScreen :: X ()
lockScreen = spawn "xscreensaver-command -lock"

-- |
-- If you want to use this, pm-suspend must be added to sudoers without inputting password
-- `xscreensaver-command -lock; sleep 2; sudo pm-suspend`
lockScreenSuspend :: X ()
lockScreenSuspend = spawn "xscreensaver-command -lock; sleep 2; sudo pm-suspend"

-- |
-- If you want to this, pm-hibernate must be added to sudoers without inputting password
-- `xscreensaver-command -lock; sleep 2; sudo pm-hibernate`
lockScreenHibernate :: X ()
lockScreenHibernate = spawn "xscreensaver-command -lock; sleep 2; sudo pm-hibernate"
