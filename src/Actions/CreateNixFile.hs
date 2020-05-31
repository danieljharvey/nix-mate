module Actions.CreateNixFile (createNixFile, createDerivation) where

import Data.Coerce
import qualified Data.List as L
import Data.Set as S
import Types.Config
import Types.CreateNixFile

type Path = String

-- create shell.nix file from our config
createNixFile :: Config -> IO ()
createNixFile cfg = do
  writeFile (coerce nixShellPath cfg) (coerce $ createDerivation cfg)

createDerivation :: Config -> Derivation
createDerivation config =
  start
    <> importPkgs (rev config) (sha256 config)
    <> packages (name config) (inputs config)

start :: Derivation
start = Derivation "let pkgs = import <nixpkgs> {}; "

importPkgs :: Rev -> Sha256 -> Derivation
importPkgs rev sha256 =
  Derivation $
    L.intercalate
      " "
      [ "packages = import (pkgs.fetchFromGitHub {",
        "owner = \"nixos\";",
        "repo = \"nixpkgs\";",
        "rev = " <> show rev <> ";",
        "sha256 = " <> show sha256 <> ";",
        "}) {};"
      ]

packages :: ProjectName -> S.Set Dependency -> Derivation
packages name deps =
  Derivation $
    concat
      [ " in packages.stdenv.mkDerivation {",
        "name = " <> show name <> ";",
        "buildInputs = with packages; [",
        depNames,
        "]; }"
      ]
  where
    depNames =
      L.intercalate " " depNameList
    depNameList =
      coerce <$> S.toList deps
