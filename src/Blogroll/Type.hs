{-# LANGUAGE DuplicateRecordFields #-}

module Blogroll.Type (FeedEntry (..), Blogroll (..), PageKind (..), RenderConfig (..), Warning (..), formatWarning) where

import Data.Text (Text)
import Data.Text qualified as T
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

data Warning
  = InvalidUrl Text
  | FetchFailed URI String
  | XmlParseFailed URI
  | InvalidEntryLink URI Text
  | InvalidEntryDate URI Text
  | FaviconFetchFailed Text String
  | FontLoadFailed FilePath String
  deriving (Show, Eq)

formatWarning :: Warning -> String
formatWarning (InvalidUrl raw) = "Skipped invalid URL: " ++ T.unpack raw
formatWarning (FetchFailed uri err) = "Failed to fetch " ++ show uri ++ ": " ++ err
formatWarning (XmlParseFailed uri) = "Failed to parse XML from " ++ show uri
formatWarning (InvalidEntryLink feedUri raw) = "Invalid entry link in " ++ show feedUri ++ ": " ++ T.unpack raw
formatWarning (InvalidEntryDate feedUri raw) = "Invalid entry date in " ++ show feedUri ++ ": " ++ T.unpack raw
formatWarning (FaviconFetchFailed domain err) = "Failed to fetch favicon for " ++ T.unpack domain ++ ": " ++ err
formatWarning (FontLoadFailed path err) = "Failed to load font at " ++ path ++ ": " ++ err