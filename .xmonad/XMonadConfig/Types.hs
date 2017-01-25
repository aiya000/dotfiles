module XMonadConfig.Types
  ( WorkspaceName
  , Workspaces
  ) where

-- | The another expression of XMonad.Config.XConfig.workspaces
type Workspaces    = [WorkspaceName]
type WorkspaceName = String
