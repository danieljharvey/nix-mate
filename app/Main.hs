module Main where

import qualified Actions.Add as Actions
import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Direnv as Actions
import qualified Actions.Search as Actions
import qualified Actions.Shell as Actions
import Data.Coerce
import Lib
import Options
import Options.Applicative
import Types.Config
import Types.Shell

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
    Derivation -> do
      cfg <- Actions.loadConfig "./nix-mate.json"
      case cfg of
        Just cfg' -> do
          putStr (Actions.createDerivation cfg')
        _ -> print "No nix-mate.json found in this folder"
    Init -> do
      Actions.init
      Actions.createDirenvRc "./.envrc"
      putStrLn "Template project created!"
    Paths -> do
      cfg <- Actions.loadConfig "./nix-mate.json"
      case cfg of
        Just cfg' ->
          Actions.getNixPaths cfg'
            >>= (putStr . coerce)
        Nothing -> print "No nix-mate.json found in this folder"
    Add dep -> do
      Actions.addPackage "./nix-mate.json" dep
  pure ()
