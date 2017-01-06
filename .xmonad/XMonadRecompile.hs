{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Shelly hiding ((</>))
import System.EasyFile (getAppUserDataDirectory)
import System.FilePath.Posix ((</>))
import System.Info (arch, os)
import Text.Printf (printf)
import qualified Data.Text as T

default (Text)


-- This script is accorded on xmonad version of 2017-01-06
main :: IO ()
main = shelly . verbosely $ do
  xmonadDir        <- liftIO $ getAppUserDataDirectory "xmonad"
  -- `T.init` removes tail '\n'
  xmonadConfigFile <- T.init <$> run "which" ["xmonad-config"]
  let xmonadFile = T.pack $ xmonadDir </> printf "xmonad-%s-%s" arch os
  run_ "cp" ["-f", xmonadConfigFile, xmonadFile]
