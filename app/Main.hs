module Main where

import qualified Actions.Add as Actions
import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Direnv as Actions
import qualified Actions.Pin as Actions
import qualified Actions.Remove as Actions
import qualified Actions.Search as Actions
import qualified Actions.Shell as Actions
import qualified Actions.Tags as Actions
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Options
import Types.Add
import Types.Config
import Types.Shell
import Types.Tags

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
      cfg <- Actions.loadConfig nixMateConfig
      let currentRev = fromMaybe mempty $ case cfg of
            Right cfg' -> Just (rev cfg')
            _ -> Nothing
      tags <- Actions.fetchTags
      let showTag t =
            if tTagHash t == currentRev
              then "> " <> Actions.displayTag t <> " <"
              else Actions.displayTag t
      traverse_ (putStrLn . showTag) tags
    Pin revision -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
        Right cfg' -> do
          newCfg <- Actions.updateRev cfg' nixMateConfig revision
          case newCfg of
            Left _ -> do
              putStrLn "Could not update config revision"
            Right newCfg' -> do
              _ <- Actions.getNixPaths newCfg'
              putStrLn $ "Updated to use revision " <> show revision
    SetVersion versionStr -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Right cfg' -> do
          tags <- Actions.fetchTags
          case Actions.matchTag versionStr tags of
            Just tag -> do
              _newCfg <- Actions.updateRev cfg' nixMateConfig (tTagHash tag)
              putStrLn $ "Updated to use nixpkgs version " <> Actions.displayTag tag
            Nothing -> do
              putStrLn $
                "Could not find a tag matching '" <> versionStr
                  <> "'. Run nix-mate tags to see what is available."
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
    UpdateVersion -> do
      cfg <- Actions.loadConfig nixMateConfig
      case cfg of
        Left CouldNotLoadConfig ->
          print "No nix-mate.json found in this folder"
        Right cfg' -> do
          tags <- Actions.fetchTags
          case Actions.getRecent tags of
            Nothing -> do
              putStrLn "Error finding a newer nixpkgs version, could not update"
            Just newestTag ->
              case Actions.findTagByHash (rev cfg') tags of
                Just currentTag ->
                  if currentTag == newestTag
                    then putStrLn "Already on most up to date version!"
                    else do
                      _newCfg <- Actions.updateRev cfg' nixMateConfig (tTagHash newestTag)
                      putStrLn $ "Updated to use nixpkgs version " <> Actions.displayTag newestTag
                Nothing -> do
                  putStrLn $ "Currently using a custom pinned nixpkgs commit: " <> displayRev (rev cfg')
                  _newCfg <- Actions.updateRev cfg' nixMateConfig (tTagHash newestTag)
                  putStrLn $ "Updated to use nixpkgs version " <> Actions.displayTag newestTag
                  putStrLn $ "To undo, run 'nix-mate pin " <> displayRev (rev cfg') <> "'"

  pure ()
