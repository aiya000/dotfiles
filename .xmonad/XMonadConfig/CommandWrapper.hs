-- | The cli command wrappers of X
module XMonadConfig.CommandWrapper
  ( takeScreenShot
  , touch
  , xmonadRestartWithMessage
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import XMonad.Core (X, spawn)
import XMonadConfig.Types (ScreenShotType (FullScreen, ActiveWindow))


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


-- | Create a file
touch :: FilePath -> X ()
touch = liftIO . flip writeFile ""

-- |
-- Restart xmonad after running xmonad was killed,
-- and Show the message of xmonad restarted.
-- xmonadRestartWithMessage depends haskell-stack, sleep and notify-send
xmonadRestartWithMessage :: X ()
xmonadRestartWithMessage =
  spawn $ "xmonad-config --recompile && " ++
          "xmonad-config --restart && " ++
          "sleep 1 && " ++
          "notify-send 'XMonad' 'Restarted'"
