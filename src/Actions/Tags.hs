module Actions.Tags (fetchTags, parseTags, displayTag) where

import Actions.Search
import Data.Maybe
import Types.Config
import Types.Tags

displayTag :: Tag -> String
displayTag (Tag tag _rev) =
  getTagName tag

fetchTags :: IO [Tag]
fetchTags = do
  str <- safeShell gitCommand ""
  pure (parseTags str)

parseTags :: String -> [Tag]
parseTags s = reverse $ catMaybes (parseRow <$> items)
  where
    items = lines s

parseRow :: String -> Maybe Tag
parseRow s = case words s of
  (rev' : tag : _) ->
    Tag
      <$> mkTagName tag
        <*> pure (Rev rev')
  _ -> Nothing

gitCommand :: String
gitCommand =
  unwords
    [ "git",
      "ls-remote",
      "--tags",
      "--sort=v:refname",
      "https://github.com/nixos/nixpkgs/"
    ]
