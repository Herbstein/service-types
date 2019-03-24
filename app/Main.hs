{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO.Global
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Text                      ( Text )
import           Debug.Trace
import           GHC.Generics
import           Lib
import           Prelude
import           System.FilePath

data TypeFile = TypeFile { name :: String, urls :: [String] }
  deriving (Show, Generic, ToJSON, FromJSON)

typeFile :: FilePath
typeFile = "definitions.json"

getJSON :: IO B.ByteString
getJSON = B.readFile typeFile

data TypeClasses = TypeClasses { classesName :: String, classes :: [Class] }
  deriving (Show)

parseTypeFile :: TypeFile -> IO TypeClasses
parseTypeFile file = TypeClasses (name file) . catMaybes <$> mapM
  parseServiceNowAPIDocumentation
  (urls file)

data TypeDefinitionFile =
  TypeDefinitionFile { fileName :: String, contents :: String }

typeClassesToFile :: TypeClasses -> TypeDefinitionFile
typeClassesToFile xs = TypeDefinitionFile (classesName xs)
  $ intercalate "\n\n" (map prettyPrint $ classes xs)

writeDefinitionFile :: FilePath -> TypeDefinitionFile -> IO ()
writeDefinitionFile basePath file =
  writeFile (basePath </> fileName file <.> "d.ts") (contents file)

main :: IO ()
main = do
  fileDecode <- eitherDecode <$> getJSON
  files      <- case fileDecode of
    Left err -> do
      putStrLn err
      return []
    Right files -> return files
  fileClasses <- parallel (map parseTypeFile files)
  parallel
      (map (writeDefinitionFile "examples" . typeClassesToFile) fileClasses)
    *> stopGlobalPool
