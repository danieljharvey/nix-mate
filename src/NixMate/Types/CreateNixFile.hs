{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NixMate.Types.CreateNixFile where

newtype Derivation = Derivation String
  deriving newtype (Show, Semigroup)
