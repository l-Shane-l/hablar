{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Directory (doesFileExist)
import System.Environment (getArgs)

-- Define the data structure for a single entry
data Entry = Entry {statement :: Text, acceptableResponses :: [Text]} deriving (Show, Generic)

-- Define the data structure for multiple entries
newtype Entries = Entries [Entry] deriving (Show, Generic)

-- Aeson instances for automatic JSON conversion
instance FromJSON Entry

instance ToJSON Entry

instance FromJSON Entries

instance ToJSON Entries

-- Function to read entries from a file, returning an empty list if the file doesn't exist
readEntries :: FilePath -> IO Entries
readEntries filePath = do
  exists <- doesFileExist filePath
  if exists
    then eitherDecode' <$> B.readFile filePath >>= either (const $ return (Entries [])) return
    else return $ Entries []

-- Function to append a new entry to the file
appendEntry :: FilePath -> Entry -> IO ()
appendEntry filePath entry = do
  Entries existingEntries <- readEntries filePath
  let updatedEntries = Entries $ existingEntries ++ [entry]
  B.writeFile filePath (encode updatedEntries)

-- Function to interact with users for each entry
interactWithEntries :: Entries -> IO ()
interactWithEntries (Entries entries) = mapM_ interactWithEntry entries
  where
    interactWithEntry (Entry stmt resps) = do
      putStrLn $ "Statement: " ++ T.unpack stmt
      putStrLn "Your response: "
      userResponse <- getLine
      if T.pack userResponse `elem` resps
        then putStrLn "Correct!\n"
        else putStrLn "Incorrect!\n"

-- Modify the main function to include the new interaction mode
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add" : stmt : resps) -> do
      let entry = Entry (T.pack stmt) (map T.pack resps)
      appendEntry "data.json" entry
      putStrLn "Entry added to data.json."
    ["read"] -> do
      entries <- readEntries "data.json"
      interactWithEntries entries
    _ -> putStrLn "Usage: \n  add <statement> <response1> [response2] ... \n  read"
