module Types.Shell (NixPaths (..)) where

newtype NixPaths = NixPaths String
  deriving (Show)
