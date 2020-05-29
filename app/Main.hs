module Main where

import qualified Actions.Add as Actions
import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Search as Actions
import Lib
import Options
import Options.Applicative
import Types.Config

main :: IO ()
main = do
  cmd <- getCmd
  case cmd of
    Search s -> do
      found <- Actions.search s
      print found
    Output -> do
      cfg <- Actions.loadConfig "./nix-mate.json"
      case cfg of
        Just cfg' -> do
          Actions.createNixFile "./shell.nix" cfg'
          print "shell.nix created"
        Nothing -> print "Could not find nix-mate.json"
    Init -> Actions.init >>= print
    Add dep -> do
      Actions.addPackage "./nix-mate.json" dep
  pure ()
