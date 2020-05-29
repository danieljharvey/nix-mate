module Actions.CreateNixFile (createNixFile) where

import qualified Data.List as L
import Types.Config

type Path = String

-- create shell.nix file from our config
createNixFile :: Path -> Config -> IO ()
createNixFile path cfg =
  writeFile path (nixShell cfg)

nixShell :: Config -> String
nixShell config =
  start
    <> importPkgs (rev config) (sha256 config)
    <> packages (name config) (inputs config)

start :: String
start = "let pkgs = import <nixpkgs> {}; "

importPkgs :: Rev -> Sha256 -> String
importPkgs rev sha256 =
  L.intercalate
    " "
    [ "packages = import (pkgs.fetchFromGitHub {",
      "owner = \"nixos\";",
      "repo = \"nixpkgs\";",
      "rev = " <> show rev <> ";",
      "sha256 = " <> show sha256 <> ";",
      "}) {};"
    ]

packages :: ProjectName -> [Dependency] -> String
packages name deps =
  concat
    [ " in packages.stdenv.mkDerivation {",
      "name = " <> show name <> ";",
      "buildInputs = with packages; [",
      depNames,
      "]; }"
    ]
  where
    depNames =
      L.intercalate " " ((\(Dependency name) -> name) <$> deps)
