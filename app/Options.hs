module Options (getCmd, Command (..)) where

import Options.Applicative
import Types.Config

data Command
  = Search Dependency
  | Add Dependency
  | Remove Dependency
  | Output
  | Init
  | Paths
  | Derivation
  | ListTags
  | Pin Rev
  | SetVersion String
  | UpdateVersion
  deriving (Eq, Ord, Show)

helpfulPreferences :: ParserPrefs
helpfulPreferences =
  defaultPrefs
    { prefShowHelpOnError = True,
      prefShowHelpOnEmpty = True
    }

getCmd :: IO Command
getCmd = customExecParser helpfulPreferences (info parseOpts idm)

parseDependency :: Parser Dependency
parseDependency =
  Dependency
    <$> argument str (metavar "<package>")

parseRevision :: Parser Rev
parseRevision =
  Rev
    <$> argument
      str
      (metavar "<nixpkgs revision hash>")

parseTagName :: Parser String
parseTagName =
  argument
    str
    (metavar "<nixpkgs version number>")

parseOpts :: Parser Command
parseOpts =
  hsubparser
    ( command
        "search"
        ( info
            (Search <$> parseDependency)
            (progDesc "Search for a package in Nix")
        )
        <> command
          "output"
          ( info
              (pure Output)
              (progDesc "Output a shell.nix file")
          )
        <> command
          "derivation"
          ( info
              (pure Derivation)
              (progDesc "Print nix derivation to stdout")
          )
        <> command
          "init"
          ( info
              (pure Init)
              (progDesc "Initialise a new project in the current folder")
          )
        <> command
          "paths"
          ( info
              (pure Paths)
              (progDesc "Install packages and return the new paths")
          )
        <> command
          "add"
          ( info
              (Add <$> parseDependency)
              (progDesc "Add a package")
          )
        <> command
          "remove"
          ( info
              (Remove <$> parseDependency)
              (progDesc "Remove a package")
          )
        <> command
          "tags"
          ( info
              (pure ListTags)
              (progDesc "List available nixpkgs versions")
          )
        <> command
          "pin"
          ( info
              (Pin <$> parseRevision)
              (progDesc "Pin project to a specific Nixpkgs revision")
          )
        <> command
          "set-version"
          ( info
              (SetVersion <$> parseTagName)
              (progDesc "Set project to use a tagged version of nixpkgs, ie '18.03'")
          )
        <> command
          "update"
          ( info
              (pure UpdateVersion)
              (progDesc "Set project to use the most recent stable version of nixpkgs")
          )
    )
