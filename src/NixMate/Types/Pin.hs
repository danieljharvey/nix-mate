module NixMate.Types.Pin where

import NixMate.Types.Config

data PinError
  = CouldNotUseNewRevision
  | CouldNotCalculateSha Rev
  deriving (Eq, Ord, Show)
