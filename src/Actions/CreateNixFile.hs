module Actions.CreateNixFile (createNixFile, createDerivation, savePackageNix) where

import Data.Coerce
import qualified Data.List as L
import Data.Set as S
import Types.Config
  ( Config (..),
    Dependency (..),
    ProjectName (..),
    Rev (..),
    Sha256 (..),
    ShellPath (..),
  )
import Types.CreateNixFile

-- create shell.nix file from our config
createNixFile :: Config -> IO ()
createNixFile cfg = do
  writeFile (coerce nixShellPath cfg) (coerce $ createDerivation cfg)

savePackageNix :: Config -> IO ()
savePackageNix cfg = do
  writeFile ("./package.nix") (coerce createPackageNix cfg)

createPackageNix :: Config -> Derivation
createPackageNix config =
  start <> importPkgs (rev config) (sha256 config) <> Derivation " in packages"

createDerivation :: Config -> Derivation
createDerivation config =
  start
    <> importPkgs (rev config) (sha256 config)
    <> packages (name config) (inputs config)

start :: Derivation
start = Derivation "let pkgs = import <nixpkgs> {}; "

importPkgs :: Rev -> Sha256 -> Derivation
importPkgs rev' sha256' =
  Derivation $
    L.intercalate
      " "
      [ "packages = import (pkgs.fetchFromGitHub {",
        "owner = \"nixos\";",
        "repo = \"nixpkgs\";",
        "rev = \"" <> coerce rev' <> "\";",
        "sha256 = \"" <> coerce sha256' <> "\";",
        "}) {};"
      ]

packages :: ProjectName -> S.Set Dependency -> Derivation
packages name' deps =
  Derivation $
    concat
      [ " in packages.stdenv.mkDerivation {",
        "name = \"" <> coerce name' <> "\";",
        "buildInputs = with packages; [",
        depNames,
        "]; }"
      ]
  where
    depNames =
      L.intercalate " " depNameList
    depNameList =
      coerce <$> S.toList deps
