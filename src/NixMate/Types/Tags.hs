{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module NixMate.Types.Tags
  ( Tag (..),
    TagName,
    mkTagName,
    getTagName,
    tagsPrefix,
  )
where

import qualified Data.Char as Char
import Data.List (isInfixOf)
import NixMate.Types.Config

newtype TagName = TagName String
  deriving newtype (Eq, Ord, Show)

tagsPrefix :: String
tagsPrefix = "refs/tags/"

mkTagName :: String -> Maybe TagName
mkTagName s =
  let clean = drop (length tagsPrefix) s
   in if not (null clean)
        && not ("{}" `isInfixOf` clean)
        && not ("backups" `isInfixOf` clean)
        && not ("alpha" `isInfixOf` clean)
        && not ("beta" `isInfixOf` clean)
        && matchesPred (\a -> Char.isDigit a || a == '.') clean
        then Just (TagName clean)
        else Nothing

matchesPred :: (Char -> Bool) -> String -> Bool
matchesPred f s = length (filter f s) == length s

getTagName :: TagName -> String
getTagName (TagName s) = s

data Tag = Tag
  { tTagName :: TagName,
    tTagHash :: Rev
  }
  deriving (Eq, Ord, Show)
