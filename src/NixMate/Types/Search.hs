{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module NixMate.Types.Search where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import GHC.Generics

data SearchError
  = CouldNotReadJson
  | NothingFound
  deriving (Eq, Ord, Show)

-- response from nix search
type SearchResponse = M.Map String SearchPackage

newtype PackageMeta = PackageMeta
  { description :: Maybe String
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

data SearchPackage = SearchPackage
  { name :: String,
    pname :: String,
    version :: String,
    system :: String,
    meta :: PackageMeta
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)
