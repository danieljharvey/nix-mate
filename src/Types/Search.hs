{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Search where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import GHC.Generics

data SearchError
  = CouldNotReadJson
  | NothingFound
  deriving (Eq, Ord, Show)

-- response from nix search
type SearchResponse = M.Map String SearchPackage

data SearchPackage
  = SearchPackage
      { name :: String,
        pname :: String,
        version :: String,
        system :: String
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)
