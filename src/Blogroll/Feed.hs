{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Feed where

import Blogroll.Error
import Blogroll.Type (FeedEntry (..))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, element, fromDocument, laxElement, ($//), (&//), (>=>))

parseFeed :: Text -> L8.ByteString -> Either BlogrollError [FeedEntry]
parseFeed siteUrl xmlContent =
  case parseLBS def xmlContent of
    Left err -> Left $ ParseError $ "Failed to parse feed XML: " <> T.pack (show err)
    Right doc ->
      let cursor = fromDocument doc
          rssEntries = parseRssEntries siteUrl cursor
          atomEntries = parseAtomEntries siteUrl cursor
       in Right (rssEntries ++ atomEntries)

parseRssEntries :: Text -> Cursor -> [FeedEntry]
parseRssEntries siteUrl cursor = mapMaybe parseEntry (cursor $// element "item")
  where
    parseEntry item = do
      let title = T.concat $ item $// element "title" &// content
          link = T.concat $ item $// element "link" &// content
          pubDateStr = T.concat $ item $// element "pubDate" &// content
      guard $ not (T.null title) && not (T.null link)
      date <- parseRssTime (T.unpack pubDateStr)
      return $ FeedEntry title link date siteUrl

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
      guard $ not (T.null title) && not (T.null link)
      date <- parseAtomTime (T.unpack dateStr)
      return $ FeedEntry title link date siteUrl

parseAtomTime :: String -> Maybe UTCTime
parseAtomTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat