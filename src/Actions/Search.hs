module Actions.Search where

import Control.Exception (try)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 as Char8
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import System.Process
import Types.Config (Dependency (..))
import Types.Search

-- run a shell action that throws
safeShell :: String -> String -> IO String
safeShell command def = do
  let myShell = (shell command) {cwd = (Just ".")}
  either <- try (readCreateProcess (myShell) "")
  case (either :: Either IOError String) of
    Right a -> pure a
    _ -> pure def

-- we have to pipe into cat to get it to finish ..?
searchPath :: Dependency -> String
searchPath (Dependency name) =
  "nix search " <> name <> " --json | cat"

decodeFromString :: String -> SearchResponse
decodeFromString =
  (fromMaybe mempty)
    . JSON.decode
    . Char8.pack

findMatch :: Dependency -> SearchResponse -> Search
findMatch (Dependency depName) resp =
  case match of
    Just dep -> Found dep
    _ -> Similar items
  where
    match = L.find (\a -> pkgName a == depName) items
    items = snd <$> M.toList resp

-- do nix search <package> --json
search :: Dependency -> IO Search
search depName =
  (findMatch depName)
    <$> decodeFromString
    <$> safeShell (searchPath depName) ""
