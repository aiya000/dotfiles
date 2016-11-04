{-# LANGUAGE DeriveDataTypeable #-}
-- Re define cmd option by cmdargs, just for me !!
module CmdOptions
 ( CommandLineOptions (..)
 , clOptions
 ) where

import System.Console.CmdArgs

data CommandLineOptions = CommandLineOptions
  { frontend    :: String
  , startonline :: Maybe Int
  , files       :: [String]
  } deriving (Show, Data, Typeable)

  
clOptions :: CommandLineOptions
clOptions = CommandLineOptions
  { frontend    = "vty"   &= help "The frontend to use"
  , startOnLine = Nothing &= help "Open the (last) file on line NUM"
  , files       = []      &= args
  }
  &= summary "yi (aiya000 - static built)"
  &= program "yi (aiya000 - static built)"
