{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics
import           Lib
import           Prelude

data TypeFile = TypeFile { name :: Text, urls :: [Text] } deriving (Show, Generic)

instance FromJSON TypeFile
instance ToJSON TypeFile



main :: IO ()
main = do
  cl <- parseServiceNowAPIDocumentation
    "https://docs.servicenow.com/bundle/jakarta-application-development/page/app-store/dev_portal/API_reference/glideRecordScoped/concept/c_GlideRecordScopedAPI.html#c_GlideRecordScopedAPI"
  putStrLn $ maybe "" prettyPrint cl
