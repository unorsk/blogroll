{-# LANGUAGE DuplicateRecordFields #-}

module Blogroll.Type (FeedEntry (..), OpmlFeed (..), FeedType (..), OpmlFeedEntry (..)) where

import Data.Text (Text)
import Data.Time (UTCTime)

data FeedType = RSS | ATOM
  deriving (Show, Eq)

data OpmlFeed = OpmlFeed
  { title :: Text,
    -- <dateModified>Mon, 11 Aug 2025 08:01:37 GMT</dateModified>
    ownerName :: Text,
    ownerEmail :: Text,
    entries :: [OpmlFeedEntry]
  }
  deriving (Show, Eq)

data OpmlFeedEntry = OpmlFeedEntry
  { feedType :: FeedType,
    text :: Text,
    title :: Text,
    url :: Text
  }
  deriving (Show, Eq)

data FeedEntry = FeedEntry
  { entryTitle :: Text,
    entryLink :: Text,
    entryDate :: UTCTime,
    entrySiteUrl :: Text
  }
  deriving (Show, Eq)