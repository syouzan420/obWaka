{-# LANGUAGE OverloadedStrings #-}
module MakeText where

import qualified Data.Text as T

import File (fileRead, fileWrite)

conv :: IO ()
conv = do
  lns <- T.lines <$> fileRead inputFile 
  let res = "module TextData (textData) where\n\nimport Data.Text (Text)\n\ntextData :: Text\ntextData = \"" <> T.intercalate "\\n" lns <> "\""
  fileWrite outputFile res

inputFile :: FilePath
inputFile = "../../assets/waka.txt"

outputFile :: FilePath
outputFile = "TextData.hs"
