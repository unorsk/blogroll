{-# LANGUAGE DuplicateRecordFields #-}

module Blogroll.Type (FeedEntry (..), Blogroll (..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Network.URI (URI)

data Blogroll = Blogroll
  { title :: Text,
    pathToFontFile :: FilePath,
    urls :: [URI]
  }
  deriving (Show, Eq)

data FeedEntry = FeedEntry
  { entryTitle :: Text,
    entryLink :: URI,
    entryDate :: UTCTime,
    entrySiteUrl :: URI
  }
  deriving (Show, Eq)