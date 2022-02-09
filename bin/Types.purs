module Types where

type FileInfo =
  { inputFile :: String
  , tidyCodegenFile :: String
  , generatedOutputFile :: String
  }
