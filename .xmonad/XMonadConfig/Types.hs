module XMonadConfig.Types
  ( ScreenShotType (..)
  , WorkspaceName
  , Workspaces
  ) where

data ScreenShotType = FullScreen | ActiveWindow

-- | The another expression of XMonad.Config.XConfig.workspaces
type Workspaces    = [WorkspaceName]
type WorkspaceName = String
