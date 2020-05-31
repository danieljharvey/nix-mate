module Actions.Shell (getNixPaths, runShell) where

-- this is where we run a nix-shell
import qualified Actions.CreateNixFile as Actions
import Control.Exception (try)
import Data.Coerce
import Data.List (intercalate)
import System.Process
import Types.Config
import Types.CreateNixFile
import Types.Shell

-- run a shell action that throws
safeShell :: String -> String -> IO String
safeShell command def = do
  let myShell = (shell command) {cwd = (Just ".")}
  either <- try (readCreateProcess (myShell) "")
  case (either :: Either IOError String) of
    Right a -> pure a
    _ -> do
      print $ "Command failed: " <> show command
      pure def

addToPath :: NixPaths -> IO ()
addToPath (NixPaths paths) = do
  let command = "export PATH=" <> paths <> ":${PATH};"
  response <- safeShell command ""
  pure ()

-- runs nix-shell, gets paths, cleans them, returns them
getNixPaths :: Config -> IO NixPaths
getNixPaths cfg =
  NixPaths
    <$> firstLine
    <$> safeShell command ""
  where
    command =
      intercalate
        " "
        [ "nix-shell",
          "--expr",
          "'" <> coerce derivation <> "'",
          "--pure",
          "--command",
          "'echo $PATH'"
        ]
    derivation = Actions.createDerivation cfg
    firstLine str = case lines str of
      (a : _) -> a
      _ -> ""

runShell :: Config -> IO ()
runShell cfg = do
  nixPaths <- getNixPaths cfg
  newPath <- print nixPaths
  addToPath nixPaths
  print "added to path!"