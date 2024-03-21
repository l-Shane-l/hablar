{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
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
data Entry = Entry {statement :: Text, acceptableResponses :: [Text]} deriving (Show, Generic, Eq)

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
-- Function to ask the user if they want to add their incorrect response
askToAddResponse :: String -> IO Bool
askToAddResponse response = do
  putStrLn "Your response was not listed as acceptable. Would you like to add it? (yes/no)"
  decision <- getLine
  return $ decision `elem` ["yes", "y", "Y", "Yes", "YES"]

-- Function to update an entry in the list and write back to the file
updateEntryInFile :: FilePath -> Entry -> [Text] -> IO ()
updateEntryInFile filePath entry newResponses = do
  Entries existingEntries <- readEntries filePath
  let updatedEntries = map (\e -> if e == entry then entry {acceptableResponses = newResponses} else e) existingEntries
  B.writeFile filePath (encode $ Entries updatedEntries)

-- Modified interactWithEntry to include functionality for adding responses
interactWithEntry :: FilePath -> Entry -> IO ()
interactWithEntry filePath entry@(Entry stmt resps) = do
  putStrLn $ "Statement: " ++ T.unpack stmt
  putStrLn "Your response: "
  userResponse <- getLine
  if T.pack userResponse `elem` resps
    then putStrLn "Correct!\n"
    else do
      putStrLn "Incorrect!\n"
      addResponse <- askToAddResponse userResponse
      when addResponse $ do
        let newResponses = resps ++ [T.pack userResponse]
        updateEntryInFile filePath entry newResponses
        putStrLn "Your response has been added."

-- Updated interactWithEntries to pass filePath
interactWithEntries :: FilePath -> Entries -> IO ()
interactWithEntries filePath (Entries entries) = mapM_ (interactWithEntry filePath) entries

-- Main function adjusted to use the updated interactWithEntries
main :: IO ()
main = do
  args <- getArgs
  let filePath = "data.json"
  case args of
    ("add" : stmt : resps) -> do
      let entry = Entry (T.pack stmt) (map T.pack resps)
      appendEntry filePath entry
      putStrLn "Entry added to data.json."
    ["read"] -> do
      entries <- readEntries filePath
      interactWithEntries filePath entries
    _ -> putStrLn "Usage: \n  add <statement> <response1> [response2] ... \n  read"
