{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Feed (parseFeed, mergeFeedEntries, tryParseDate, rssDateFormats, atomDateFormats) where

import Blogroll.Type (FeedEntry (..), Warning (..))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Network.URI (URI, parseURI)
import Text.XML (def, parseLBS)
import Text.XML.Cursor (Cursor, attribute, content, fromDocument, laxElement, ($//), (&//))

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
  let (warnings, entries) = partitionEithers $ map parseItem $ cursor $// laxElement "item"
   in (entries, warnings)
  where
    parseItem item =
      let title = T.concat $ item $// laxElement "title" &// content
          link = T.concat $ item $// laxElement "link" &// content
          pubDateStr = T.concat $ item $// laxElement "pubDate" &// content
       in makeEntry siteUrl title link rssDateFormats pubDateStr

parseAtomEntries :: URI -> Cursor -> ([FeedEntry], [Warning])
parseAtomEntries siteUrl cursor =
  let (warnings, entries) = partitionEithers $ map parseEntry $ cursor $// laxElement "entry"
   in (entries, warnings)
  where
    parseEntry entry =
      let title = T.concat $ entry $// laxElement "title" &// content
          link = maybe "" T.concat $ entryAlternateLink entry
          published = T.concat $ entry $// laxElement "published" &// content
          updated = T.concat $ entry $// laxElement "updated" &// content
          dateStr = if T.null published then updated else published
       in makeEntry siteUrl title link atomDateFormats dateStr

-- An Atom entry may carry several <link> elements (alternate, self, replies, ...).
-- Per the spec, a link without rel is an alternate.
entryAlternateLink :: Cursor -> Maybe [Text]
entryAlternateLink entry =
  let links = entry $// laxElement "link"
      isAlternate c = attribute "rel" c `elem` [[], ["alternate"]]
   in listToMaybe $ map (attribute "href") $ filter isAlternate links ++ links

makeEntry :: URI -> Text -> Text -> [String] -> Text -> Either Warning FeedEntry
makeEntry siteUrl title link dateFormats dateStr =
  case (parseURI (T.unpack link), tryParseDate dateFormats (T.unpack dateStr)) of
    (Nothing, _) -> Left $ InvalidEntryLink siteUrl link
    (_, Nothing) -> Left $ InvalidEntryDate siteUrl dateStr
    (Just uriLink, Just date) -> Right $ FeedEntry title uriLink date siteUrl

tryParseDate :: [String] -> String -> Maybe UTCTime
tryParseDate formats dateStr =
  asum [parseTimeM True defaultTimeLocale fmt dateStr | fmt <- formats]

rssDateFormats :: [String]
rssDateFormats =
  [ "%a, %d %b %Y %H:%M:%S %Z",
    "%a, %d %b %Y %H:%M:%S %z",
    "%a, %d %b %Y %H:%M %Z",
    "%d %b %Y %H:%M:%S %z"
  ]

atomDateFormats :: [String]
atomDateFormats =
  [ "%Y-%m-%dT%H:%M:%S%QZ",
    "%Y-%m-%dT%H:%M:%S%Q%z",
    "%Y-%m-%dT%H:%M:%S%Q%Z",
    "%Y-%m-%d"
  ]

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortOn (Down . entryDate) . concat
