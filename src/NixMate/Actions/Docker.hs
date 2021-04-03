module NixMate.Actions.Docker (createDocker, doDocker) where

-- this is where we run a nix-shell
import Data.Coerce
import qualified Data.List as L
import qualified Data.Set as S
import NixMate.Shared
import NixMate.Types.Config

createDocker :: Config -> String
createDocker cfg = dockerImports cfg <> createDockerDerivation cfg

dockerImports :: Config -> String
dockerImports cfg =
  L.unwords
    [ "let pkgs = import <nixpkgs> {};",
      "packages = import (pkgs.fetchFromGitHub {",
      "owner = \"nixos\";",
      "repo = \"nixpkgs\";",
      "rev = \"" <> coerce (rev cfg) <> "\";",
      "sha256 = \"" <> coerce (sha256 cfg) <> "\";",
      "}) { system = \"x86_64-linux\"; };"
    ]

createDockerDerivation :: Config -> String
createDockerDerivation cfg =
  L.unwords
    [ " in packages.dockerTools.buildLayeredImage {",
      "name = \"" <> coerce (name cfg) <> "\";",
      "tag = \"latest\";",
      "contents = with packages; [",
      depNames,
      "];",
      "maxLayers = 120; }\n"
    ]
  where
    depNames =
      L.unwords depNameList
    depNameList =
      coerce <$> S.toList (inputs cfg)

-- runs nix-shell, gets paths, cleans them, returns them
doDocker :: Config -> IO ()
doDocker cfg = do
  a <- safeShell command
  case a of
    Left e -> print e
    Right _ -> do
      b <- safeShell "docker load < ./result"
      case b of
        Left e -> print e
        Right b' -> putStrLn b'
  where
    command =
      unwords
        [ "nix-build",
          "--expr",
          "'" <> coerce derivation <> "'"
        ]
    derivation = createDocker cfg
