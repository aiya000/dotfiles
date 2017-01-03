-- | The X actions
module XMonadConfig.Actions where

import XMonad.Actions.Workscreen (shiftToWorkscreen)
import XMonad.Core (X, ScreenId (S))
import XMonadConfig.Types (Workspaces)


-- |
-- Move the current window to the target workspace,
-- The target worspace must be contain Workspaces
moveWindowTo :: Workspaces -> ScreenId -> X ()
moveWindowTo workspaces (S n) =
  let workscreenId = (n - 1) `mod` length workspaces
  in shiftToWorkscreen workscreenId
