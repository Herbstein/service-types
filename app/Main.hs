module Main where

import           Data.Maybe
import           Lib

main :: IO ()
main = do
  cl <- parseServiceNowAPIDocumentation
    "https://docs.servicenow.com/bundle/jakarta-application-development/page/app-store/dev_portal/API_reference/glideRecordScoped/concept/c_GlideRecordScopedAPI.html#c_GlideRecordScopedAPI"
  putStrLn $ maybe "" prettyPrint cl
