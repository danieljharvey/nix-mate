module NixMate.Actions.Test where

import Data.Coerce
import qualified NixMate.Actions.CreateNixFile as Actions
import NixMate.Shared (safeShell)
import NixMate.Types.Config
import NixMate.Types.CreateNixFile

-- instead of looking up a package
-- we just smash it into a derivation and see what Nix
-- has to say about that

testCommand :: Derivation -> String
testCommand derivation =
  unwords
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
