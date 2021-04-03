module NixMate.Actions.Search (search, displayPackage, safeShell) where

import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Char as Ch
import Data.Coerce
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified NixMate.Actions.CreateNixFile as Actions
import NixMate.Shared
import NixMate.Types.Config
import NixMate.Types.Search

-- we have two methods of searching
-- nix search <package> - which is cached nicely but does not look deep into
-- stuff like haskellPackages
-- it would not find Agda, for instance, which is haskellPackages.Agda

-- -qa --description
searchDescription :: Dependency -> String
searchDescription depName =
  L.unwords
    [ "nix-env",
      "-qaP",
      "--file",
      "package.nix",
      "--available",
      "--json",
      "'.*" <> coerce depName <> ".*'",
      "|",
      "cat"
    ]

decodeFromString :: String -> Maybe SearchResponse
decodeFromString =
  JSON.decode
    . Char8.pack
    . L.filter Ch.isAscii

findMatch ::
  SearchResponse ->
  Either SearchError [(Dependency, SearchPackage)]
findMatch resp =
  case items of
    (_ : _) -> Right items
    _ -> Left NothingFound
  where
    items = first Dependency <$> M.toList resp

-- do nix search <package> --json
search ::
  Config ->
  Dependency ->
  IO (Either SearchError [(Dependency, SearchPackage)])
search cfg depName = do
  Actions.savePackageNix cfg
  str <- safeShellWithDefault (searchDescription depName) ""
  case decodeFromString str of
    Just items -> pure (findMatch items)
    _ -> pure (Left CouldNotReadJson)

displayPackage :: (Dependency, SearchPackage) -> String
displayPackage (depName, details) =
  ":: " <> coerce depName <> "\n"
    <> "Package:     "
    <> pname details
    <> "\n"
    <> "Version:     "
    <> version details
    <> "\n"
    <> "Description: "
    <> fromMaybe
      "n/a"
      ( (description . meta) details
      )
    <> "\n"
    <> ">> nix-mate add "
    <> coerce depName
    <> "\n"
