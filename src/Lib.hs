module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Response
  = Response
      { code :: Int,
        message :: String
      }

addPackage :: String -> IO Response
addPackage = undefined
