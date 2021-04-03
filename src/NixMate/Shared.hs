module NixMate.Shared (safeShell, safeShellWithDefault) where

import Control.Exception (try)
import System.Process

-- run a shell action that throws
safeShellWithDefault :: String -> String -> IO String
safeShellWithDefault command def = do
  res <- safeShell command
  case res of
    Right a -> pure a
    Left _ -> pure def

-- run a shell action that throws
-- todo, better capture errors
safeShell :: String -> IO (Either String String)
safeShell command = do
  let myShell = (shell command) {cwd = Just "."}
  either' <- try (readCreateProcess myShell "")
  case (either' :: Either IOError String) of
    Right a -> pure (Right a)
    Left e -> pure (Left (show e))
