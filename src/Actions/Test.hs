module Actions.Test where

import qualified Actions.CreateNixFile as Actions
import Control.Exception (try)
import Data.Coerce
import Data.List (intercalate)
import System.Process
import Types.Config
import Types.CreateNixFile

-- instead of looking up a package
-- we just smash it into a derivation and see what Nix
-- has to say about that

-- run a shell action that throws
-- todo, better capture errors
safeShell :: String -> IO (Either String String)
safeShell command = do
  let myShell = (shell command) {cwd = (Just ".")}
  either <- try (readCreateProcess (myShell) "")
  case (either :: Either IOError String) of
    Right a -> pure (Right a)
    Left e -> pure (Left (show e))

testCommand :: Derivation -> String
testCommand derivation =
  intercalate
    " "
    [ "nix-instantiate",
      "--dry-run",
      "--eval",
      "--no-build-output",
      "--json",
      "--strict",
      "-E",
      "'" <> coerce derivation <> "'"
    ]

testDerivation :: Config -> IO (Either String String)
testDerivation cfg = do
  let derivation = Actions.createDerivation cfg
  safeShell (testCommand derivation)
