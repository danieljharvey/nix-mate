module Options (getCmd, Command (..)) where

import Options.Applicative
import Types.Config

data Command
  = Search Dependency
  | Add Dependency
  | Output
  | Init
  deriving (Eq, Ord, Show)

getCmd :: IO Command
getCmd = execParser (info opts idm)

searchDep :: Parser Dependency
searchDep =
  (Dependency)
    <$> argument str (metavar "<package>")

opts :: Parser Command
opts =
  subparser
    ( command
        "search"
        ( info
            (Search <$> searchDep)
            (progDesc "Search for a package in Nix")
        )
        <> command
          "output"
          ( info
              (pure Output)
              (progDesc "Output a shell.nix file")
          )
        <> command
          "init"
          ( info
              (pure Init)
              (progDesc "Initialise a new project in the current folder")
          )
        <> command
          "add"
          ( info
              (Add <$> searchDep)
              (progDesc "Add a package")
          )
    )
