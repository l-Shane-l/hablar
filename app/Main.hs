{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM, when)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.Printf (printf)

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

-- Now returns True for correct answers, False otherwise
interactWithEntry :: FilePath -> Entry -> IO Bool
interactWithEntry filePath entry@(Entry stmt resps) = do
  putStrLn $ "Statement: " ++ T.unpack stmt
  putStrLn "Your response: "
  userResponse <- getLine
  if T.pack userResponse `elem` resps
    then do
      putStrLn "Correct!\n"
      return True
    else do
      putStrLn "Incorrect!\n"
      addResponse <- askToAddResponse userResponse
      when addResponse $ do
        let newResponses = resps ++ [T.pack userResponse]
        updateEntryInFile filePath entry newResponses
        putStrLn "Your response has been added."
      return False

-- Accumulates and returns scoring information
interactWithEntries :: FilePath -> Entries -> IO (Int, Int)
interactWithEntries filePath (Entries entries) =
  foldl
    ( \acc entry -> do
        (total, correct) <- acc
        result <- interactWithEntry filePath entry
        return (total + 1, correct + if result then 1 else 0)
    )
    (return (0, 0))
    entries

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
      (totalQuestions, correctAnswers) <- interactWithEntries filePath entries
      let percentageCorrect =
            if totalQuestions > 0
              then (fromIntegral correctAnswers / fromIntegral totalQuestions :: Double) * 100
              else 0 :: Double

      putStrLn $ "Total Questions: " ++ show totalQuestions ++ ", Correct Answers: " ++ show correctAnswers ++ ", Percentage Correct: " ++ printf "%.2f%%" percentageCorrect
    _ -> putStrLn "Usage: \n  add <statement> <response1> [response2] ... \n  read"
