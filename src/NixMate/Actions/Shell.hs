module NixMate.Actions.Shell (getNixPaths) where

-- this is where we run a nix-shell

import Data.Coerce
import qualified NixMate.Actions.CreateNixFile as Actions
import NixMate.Shared (safeShellWithDefault)
import NixMate.Types.Config
import NixMate.Types.CreateNixFile
import NixMate.Types.Shell

-- runs nix-shell, gets paths, cleans them, returns them
getNixPaths :: Config -> IO NixPaths
getNixPaths cfg =
  NixPaths
    . firstLine
    <$> safeShellWithDefault command ""
  where
    command =
      unwords
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
