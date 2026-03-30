{-# LANGUAGE DuplicateRecordFields #-}

module Blogroll.Type (FeedEntry (..), Blogroll (..), PageKind (..), RenderConfig (..)) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Network.URI (URI)

data Blogroll = Blogroll
  { title :: Text,
    pathToFontFile :: Maybe FilePath,
    recentCount :: Int,
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

data PageKind = RecentPage | AllPostsPage
  deriving (Show, Eq)

data RenderConfig = RenderConfig
  { pageKind :: PageKind,
    pageTitle :: Text,
    faviconCss :: Text,
    fontBase64 :: Maybe Text
  }
  deriving (Show, Eq)