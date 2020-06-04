module Actions.Direnv (createDirenvRc, reloadDirenv) where

import Control.Exception (try)
import System.Process (cwd, readCreateProcess, shell)
import Types.Direnv

-- here we create an .envrc which adds our new apps to the $PATH
type Path = String

createDirenvRc :: Path -> IO (Either DirenvError ())
createDirenvRc path = do
  result <- try $ writeFile path "PATH_add $(nix-mate paths)"
  case (result :: Either IOError ()) of
    Right _ -> pure (Right ())
    Left _ -> pure (Left CouldNotWriteEnvrc)

-- run a shell action that throws
safeShell :: String -> IO (Maybe String)
safeShell command = do
  let myShell = (shell command) {cwd = (Just ".")}
  either' <- try (readCreateProcess (myShell) "")
  case (either' :: Either IOError String) of
    Right a -> pure (Just a)
    _ -> do
      pure Nothing

reloadDirenv :: IO (Either DirenvError ())
reloadDirenv = do
  res <- safeShell "direnv reload"
  case res of
    Just _ -> pure (Right ())
    _ -> pure (Left CouldNotReloadDirenv)
