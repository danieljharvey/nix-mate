{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Search where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import GHC.Generics

data Search
  = Found SearchPackage
  | Similar [SearchPackage]
  deriving (Eq, Ord, Show)

type SearchResponse = M.Map String SearchPackage

-- response from nix search
data SearchPackage
  = SearchPackage
      { pkgName :: String,
        version :: String,
        description :: String
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)
