{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseServiceNowAPIDocumentation
  , prettyPrint
  , Class(..)
  )
where

import           Control.Applicative
import           Control.Monad
import           Data.AssocList.List.Predicate
import           Data.AssocList.List.Concept
import           Data.Functor.Contravariant
import           Data.List
import           Data.Maybe
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , replace
                                                , strip
                                                )
import           Text.HTML.Scalpel

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Class = Class String [Method]
  deriving (Show)

data Method = Method String [Parameter] String
  deriving (Show)

data Parameter = Parameter String String
  deriving (Show)

prettyPrint :: Class -> String
prettyPrint (Class name methods) =
  "declare class "
    ++ name
    ++ "{ \n"
    ++ intercalate "\n" (map prettyPrintMethod methods)
    ++ "\n}"

prettyPrintMethod :: Method -> String
prettyPrintMethod (Method name parameters ret) =
  name
    ++ "("
    ++ intercalate ", " (map prettyPrintParameter parameters)
    ++ "): "
    ++ ret
    ++ ";"

prettyPrintParameter :: Parameter -> String
prettyPrintParameter (Parameter name typ) = name ++ ": " ++ typ

assocSub :: Eq a => AssocList a a -> a -> a
assocSub list x = fromMaybe x $ lookupFirst (Predicate (== x)) list

validateName :: String -> String
validateName name = assocSub nameMap name
  where nameMap = [("function", "func")]

validateType :: String -> String
validateType typ = assocSub typeMap typ
  where typeMap = [("Array", "Object[]")]

className :: Scraper String String
className =
  fmap (validateName . unpack . strip . pack . takeWhile (\c -> not $ c == '-'))
    $  text
    $  "div"
    @: [hasClass "article__head"]
    // "h2"

methodName :: Scraper String String
methodName = text $ "h2" @: [hasClass "title"]

methodParameter :: Scraper String Parameter
methodParameter = do
  e <- entries
  let name = validateName $ takeWhile (\c -> not $ c == '(') $ e !! 0
  let typ  = validateType $ e !! 1
  return $ Parameter name typ
  where entries = texts $ AnyTag @: [hasClass "entry"]

methodParameters :: Scraper String [Parameter]
methodParameters = chroots
  ("table" @: [hasClass "parameters"] // "tr" @: [hasClass "row"])
  (do
    parameter <- methodParameter
    guard $ case parameter of
      Parameter "None" _ -> False
      _                  -> True
    return parameter
  )

methodReturn :: Scraper String (Maybe String)
methodReturn = do
  tableEntries <-
    chroots ("table" @: [hasClass "returns"] // "tr" @: [hasClass "row"])
    $  text
    $  "td"
    @: [hasClass "entry"]
  return $ if length tableEntries > 0
    then Just $ validateType $ tableEntries !! 0
    else Nothing

method :: Scraper String Method
method = do
  n          <- methodName
  parameters <- methodParameters
  ret        <- methodReturn
  let name =
        tail
          . takeWhile (\c -> not $ c == '(')
          . dropWhile (\c -> not $ c == '-')
          $ n
  return $ Method
    name
    (tail parameters)
    ((unpack . strip . replace "Scoped" "" . pack) $ fromMaybe "void" ret)

methods :: Scraper String [Method]
methods = chroots ("article" @: [hasClass "api-method"]) method

parseServiceNowAPIDocumentation :: String -> IO (Maybe Class)
parseServiceNowAPIDocumentation url = scrapeURL
  url
  (do
    name    <- className
    methods <- methods
    return $ Class name methods
  )

