module Actions.Search (search, displayPackage, safeShell) where

import qualified Actions.CreateNixFile as Actions
import Control.Exception (try)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Char as Ch
import Data.Coerce
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Process
import Types.Config
import Types.Search

-- we have two methods of searching
-- nix search <package> - which is cached nicely but does not look deep into
-- stuff like haskellPackages
-- it would not find Agda, for instance, which is haskellPackages.Agda

-- run a shell action that throws
safeShell :: String -> String -> IO String
safeShell command def = do
  let myShell = (shell command) {cwd = Just "."}
  either' <- try (readCreateProcess myShell "")
  case (either' :: Either IOError String) of
    Right a ->
      pure a
    _ ->
      pure def

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
  str <- safeShell (searchDescription depName) ""
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
