{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Feed where

import Blogroll.Type (FeedEntry (..), Warning (..))
import Control.Applicative ((<|>))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Network.URI (URI, parseURI)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, element, fromDocument, laxElement, ($//), (&//), (>=>))

parseFeed :: URI -> L8.ByteString -> ([FeedEntry], [Warning])
parseFeed siteUrl xmlContent =
  case parseLBS def xmlContent of
    Left _ -> ([], [XmlParseFailed siteUrl])
    Right doc ->
      let cursor = fromDocument doc
          (rssEntries, rssWarnings) = parseRssEntries siteUrl cursor
          (atomEntries, atomWarnings) = parseAtomEntries siteUrl cursor
       in (rssEntries ++ atomEntries, rssWarnings ++ atomWarnings)

parseRssEntries :: URI -> Cursor -> ([FeedEntry], [Warning])
parseRssEntries siteUrl cursor =
  let (warnings, entries) = partitionEithers $ map parseItem $ cursor $// element "item"
   in (entries, warnings)
  where
    parseItem item =
      let title = T.concat $ item $// element "title" &// content
          link = T.concat $ item $// element "link" &// content
          pubDateStr = T.concat $ item $// element "pubDate" &// content
       in case (parseURI (T.unpack link), parseRssTime (T.unpack pubDateStr)) of
            (Nothing, _) -> Left $ InvalidEntryLink siteUrl link
            (_, Nothing) -> Left $ InvalidEntryDate siteUrl pubDateStr
            (Just uriLink, Just rssDate) -> Right $ FeedEntry title uriLink rssDate siteUrl

parseRssTime :: String -> Maybe UTCTime
parseRssTime dateStr =
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" dateStr

parseAtomEntries :: URI -> Cursor -> ([FeedEntry], [Warning])
parseAtomEntries siteUrl cursor =
  let (warnings, entries) = partitionEithers $ map parseEntry $ cursor $// laxElement "entry"
   in (entries, warnings)
  where
    parseEntry entry =
      let title = T.concat $ entry $// laxElement "title" &// content
          link = T.concat $ entry $// laxElement "link" >=> attribute "href"
          published = T.concat $ entry $// laxElement "published" &// content
          updated = T.concat $ entry $// laxElement "updated" &// content
          dateStr = if T.null published then updated else published
       in case (parseURI (T.unpack link), parseAtomTime (T.unpack dateStr)) of
            (Nothing, _) -> Left $ InvalidEntryLink siteUrl link
            (_, Nothing) -> Left $ InvalidEntryDate siteUrl dateStr
            (Just uriLink, Just date) -> Right $ FeedEntry title uriLink date siteUrl

parseAtomTime :: String -> Maybe UTCTime
parseAtomTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat