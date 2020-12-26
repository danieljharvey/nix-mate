module Main where

import qualified Actions.Add as Actions
import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Direnv as Actions
import qualified Actions.Remove as Actions
import qualified Actions.Search as Actions
import qualified Actions.Shell as Actions
import qualified Actions.Tags as Actions
import Data.Coerce
import Data.Foldable (traverse_)
import Options
import Types.Add
import Types.Config
import Types.Shell

nixMateConfig :: String
nixMateConfig = "./nix-mate.json"

direnvConfig :: String
direnvConfig = "./.envrc"

main :: IO ()
main = do
  cmd <- getCmd
  case cmd of
    Search s -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' -> do
          found <- Actions.search cfg' s
          case found of
            Left _ -> putStrLn "No matching packages found"
            Right packages -> do
              putStrLn $ show (length packages) <> " packages found\n"
              traverse_ (putStrLn . Actions.displayPackage) packages
        Left CouldNotLoadConfig ->
          print "Could not find nix-mate.json"
    Output -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' -> do
          Actions.createNixFile cfg'
          print "shell.nix created"
        Left CouldNotLoadConfig ->
          print "Could not find nix-mate.json"
    Derivation -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' -> do
          print (Actions.createDerivation cfg')
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
    Init -> do
      cfg <- Actions.init
      _ <- Actions.createDirenvRc direnvConfig
      _ <- Actions.getNixPaths cfg
      putStrLn "Template project created!"
    Paths -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' ->
          Actions.getNixPaths cfg'
            >>= (putStr . coerce)
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
    Add dep -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' -> do
          newCfg <- Actions.addPackage cfg' nixMateConfig dep
          case newCfg of
            Right newCfg' -> do
              _ <- Actions.getNixPaths newCfg'
              putStrLn $ "Package " <> coerce dep <> " installed!"
            Left (CouldNotFindPackage depName) ->
              print $ "Could not find package " <> coerce depName
            Left (AlreadyExistsInPackageSet depName) ->
              print $ "Package " <> coerce depName <> " already in set"
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
    Remove dep -> do
      newCfg <- Actions.removePackage nixMateConfig dep
      _ <- Actions.getNixPaths newCfg
      putStr $ "Package " <> coerce dep <> " removed"
    ListTags -> do
      tags <- Actions.fetchTags
      traverse_ (putStrLn . Actions.displayTag) tags
  pure ()
