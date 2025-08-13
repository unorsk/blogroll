{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Feed where

import Blogroll.Type (FeedEntry (..))
import Control.Applicative ((<|>))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, element, fromDocument, laxElement, ($//), (&//), (>=>))

parseFeed :: Text -> L8.ByteString -> [FeedEntry]
parseFeed siteUrl xmlContent =
  case parseLBS def xmlContent of
    Left _ -> []
    Right doc ->
      let cursor = fromDocument doc
       in parseRssEntries siteUrl cursor ++ parseAtomEntries siteUrl cursor

parseRssEntries :: Text -> Cursor -> [FeedEntry]
parseRssEntries siteUrl cursor = do
  item <- cursor $// element "item"
  let title = T.concat $ item $// element "title" &// content
      link = T.concat $ item $// element "link" &// content
      pubDateStr = T.concat $ item $// element "pubDate" &// content
  case parseRssTime (T.unpack pubDateStr) of
    Just date -> [FeedEntry title link date siteUrl]
    Nothing -> []

parseRssTime :: String -> Maybe UTCTime
parseRssTime dateStr =
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" dateStr

parseAtomEntries :: Text -> Cursor -> [FeedEntry]
parseAtomEntries siteUrl cursor = mapMaybe parseEntry (cursor $// laxElement "entry")
  where
    parseEntry entry = do
      let title = T.concat $ entry $// laxElement "title" &// content
          linkHref = listToMaybe $ entry $// laxElement "link" >=> attribute "href"
          link = fromMaybe "" linkHref
          published = T.concat $ entry $// laxElement "published" &// content
          updated = T.concat $ entry $// laxElement "updated" &// content
          dateStr = if T.null published then updated else published
      date <- parseAtomTime (T.unpack dateStr)
      return $ FeedEntry title link date siteUrl

parseAtomTime :: String -> Maybe UTCTime
parseAtomTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat