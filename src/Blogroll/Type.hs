{-# LANGUAGE DuplicateRecordFields #-}

module Blogroll.Type (FeedEntry (..), Blogroll (..)) where

import Data.Text (Text)
import Data.Time (UTCTime)

data Blogroll = Blogroll
  { title :: Text,
    urls :: [Text]
  }
  deriving (Show, Eq)

data FeedEntry = FeedEntry
  { entryTitle :: Text,
    entryLink :: Text,
    entryDate :: UTCTime,
    entrySiteUrl :: Text
  }
  deriving (Show, Eq)