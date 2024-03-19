{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import System.Environment (getArgs)

data Entry = Entry {statement :: Text, acceptableResponses :: [Text]} deriving (Show, Generic)

instance FromJSON Entry

instance ToJSON Entry

writeEntry :: Entry -> IO ()
writeEntry entry = B.writeFile "data.json" (encode entry)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [s, r] -> do
      let entry = Entry (T.pack s) (map T.pack $ words r)
      writeEntry entry
      putStrLn "Entry written to data.json"
    _ -> putStrLn "Usage: <statement> <response1 response2 ...>"
