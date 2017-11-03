{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Hpack.Extension (
  ensure
#ifdef TEST
, Defaults(..)
, parseDefaults
, Result(..)
, ensureFile
#endif
) where

import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import           System.FilePath
import           System.Directory

type URL = String

data Defaults = Defaults {
  defaultsUser :: String
, defaultsRef :: String
} deriving (Eq, Show)

parseDefaults :: String -> Maybe Defaults
parseDefaults input =  case break (== '-') $ reverse input of
  (reverse -> ref@(_:_), '-' : (reverse -> user@(_:_))) -> Just (Defaults user ref)
  _ -> Nothing

defaultsUrl :: Defaults -> URL
defaultsUrl Defaults{..} = "https://raw.githubusercontent.com/" ++ defaultsUser ++ "/hpack-template/" ++ defaultsRef ++ "/defaults.yaml"

defaultsPath :: FilePath -> Defaults -> FilePath
defaultsPath dir Defaults{..} = dir </> "defaults" </> defaultsUser ++ "-" ++ defaultsRef

data Result = Found | NotFound | Failed String
  deriving (Eq, Show)

get :: URL -> FilePath -> IO Result
get url file = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  case responseStatus response of
    Status 200 _ -> LB.writeFile file (responseBody response) >> return Found
    Status 404 _ -> return NotFound
    status -> return (Failed $ "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")")

formatStatus :: Status -> String
formatStatus (Status code message) = show code ++ " " ++ B.unpack message

ensure :: FilePath -> String -> IO (Either String FilePath)
ensure dir d = case parseDefaults d of
  Nothing -> return (Left invalid)
  Just defaults -> do
    ensureFile file url >>= \ case
      Found -> return (Right file)
      NotFound -> return (Left notFound)
      Failed err -> return (Left err)
    where
      url = defaultsUrl defaults
      file = defaultsPath dir defaults
      notFound = invalid ++ " (file " ++ url ++ " does not exist)"
  where
    invalid = "Invalid value " ++ show d ++ " for \"defaults\"!"

ensureFile :: FilePath -> URL -> IO Result
ensureFile file url = do
  createDirectoryIfMissing True (takeDirectory file)
  doesFileExist file >>= \ case
    True -> return Found
    False -> get url file
