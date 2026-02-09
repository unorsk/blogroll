{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Feed where

import Blogroll.Type (FeedEntry (..))
import Control.Applicative ((<|>))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Network.URI (URI, parseURI)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, element, fromDocument, laxElement, ($//), (&//), (>=>))

parseFeed :: URI -> L8.ByteString -> [FeedEntry]
parseFeed siteUrl xmlContent =
  case parseLBS def xmlContent of
    Left _ -> []
    Right doc ->
      let cursor = fromDocument doc
       in parseRssEntries siteUrl cursor ++ parseAtomEntries siteUrl cursor

-- TODO this one swallows all Nothing when parsing uri/time, might want to do smth about it
parseRssEntries :: URI -> Cursor -> [FeedEntry]
parseRssEntries siteUrl cursor = mapMaybe parseItem $ cursor $// element "item"
  where
    parseItem item = do
      let title = T.concat $ item $// element "title" &// content
          link = T.concat $ item $// element "link" &// content
          pubDateStr = T.concat $ item $// element "pubDate" &// content
      uriLink <- parseURI (T.unpack link)
      rssDate <- parseRssTime (T.unpack pubDateStr)
      return $ FeedEntry title uriLink rssDate siteUrl

parseRssTime :: String -> Maybe UTCTime
parseRssTime dateStr =
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" dateStr

-- TODO this one swallows all Nothing when parsing uri/time, might want to do smth about it
parseAtomEntries :: URI -> Cursor -> [FeedEntry]
parseAtomEntries siteUrl cursor = mapMaybe parseEntry (cursor $// laxElement "entry")
  where
    parseEntry entry = do
      let title = T.concat $ entry $// laxElement "title" &// content
          link = T.concat $ entry $// laxElement "link" >=> attribute "href"
          published = T.concat $ entry $// laxElement "published" &// content
          updated = T.concat $ entry $// laxElement "updated" &// content
          dateStr = if T.null published then updated else published
      uriLink <- parseURI (T.unpack link)
      date <- parseAtomTime (T.unpack dateStr)
      return $ FeedEntry title uriLink date siteUrl

parseAtomTime :: String -> Maybe UTCTime
parseAtomTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat