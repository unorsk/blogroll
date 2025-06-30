{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
module Main where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time
import Network.HTTP.Simple
import Text.XML as XML
import Text.XML.Cursor
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Control.Exception (try, SomeException)
import Control.Applicative ((<|>))

data FeedEntry = FeedEntry
  { entryTitle :: Text
  , entryLink :: Text
  , entryDate :: UTCTime
  , entrySiteUrl :: Text
  } deriving (Show, Eq)

readBlogroll :: FilePath -> IO [Text]
readBlogroll path = do
  cont <- TIO.readFile path
  return $ filter (not . T.null) $ map T.strip $ T.lines cont

fetchFeed :: Text -> IO (Either String L8.ByteString)
fetchFeed url = do
  result <- try $ do
    request <- parseRequest (T.unpack url)
    response <- httpLBS request
    return $ getResponseBody response
  case result of
    Left e -> return $ Left $ show (e :: SomeException)
    Right body -> return $ Right body

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
  case parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (T.unpack pubDateStr) of
    Just date -> return $ FeedEntry title link date siteUrl
    Nothing -> []

parseAtomEntries :: Text -> Cursor -> [FeedEntry]
parseAtomEntries siteUrl cursor = mapMaybe parseEntry (cursor $// laxElement "entry")
  where
    parseEntry entry = do
      let title = T.concat $ entry $// laxElement "title" &// content
          linkHref = listToMaybe $ entry $// laxElement "link" >=> attribute "href"
          link = maybe "" id linkHref
          published = T.concat $ entry $// laxElement "published" &// content
          updated = T.concat $ entry $// laxElement "updated" &// content
          dateStr = if T.null published then updated else published
      date <- parseAtomTime (T.unpack dateStr)
      return $ FeedEntry title link date siteUrl

parseAtomTime :: String -> Maybe UTCTime
parseAtomTime dateStr = 
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr <|>
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr <|>
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat

renderHtml :: [FeedEntry] -> Text
renderHtml entries = 
  let entriesHtml = T.concat $ map renderEntry entries
  in """
<!DOCTYPE html>
<html><head>
<title>RSS Reader</title>
<style>
@font-face {
  font-family: 'IBM Plex Sans';
  src: url('IBMPlexSans-VariableFont_wdth,wght.ttf') format('truetype');
  font-weight: 100 900;
  font-stretch: 75% 125%;
}
body {
  font-family: 'IBM Plex Sans', -apple-system, BlinkMacSystemFont, sans-serif;
  line-height: 1.6;
  max-width: 800px;
  margin: 0 auto;
  padding: 20px;
  color: #333;
}
h1 {
  color: #2c3e50;
  border-bottom: 2px solid #3498db;
  padding-bottom: 10px;
}
ul {
  list-style: none;
  padding: 0;
}
li {
  margin: 12px 0;
  padding: 8px;
  border-left: 3px solid #3498db;
  background: #f8f9fa;
}
a {
  color: #2980b9;
  text-decoration: none;
  font-weight: 500;
}
a:hover {
  text-decoration: underline;
}
.date {
  color: #7f8c8d;
  font-size: 0.9em;
}
.source {
  color: #95a5a6;
  font-size: 0.8em;
}
</style>
</head><body>
<h1>Latest Posts</h1>
<ul>
""" <> entriesHtml <> """
</ul>
</body></html>
"""
  where
    renderEntry entry = T.concat
      [ "<li><a href=\"", entryLink entry, "\">"
      , entryTitle entry, "</a><br>"
      , "<span class=\"date\">", T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (entryDate entry), "</span> "
      , "<span class=\"source\">(", entrySiteUrl entry, ")</span></li>"
      ]

main :: IO ()
main = do
  urls <- readBlogroll "blogroll.txt"
  putStrLn $ "Found " ++ show (length urls) ++ " feeds"
  
  feeds <- mapM fetchFeed urls
  let feedEntries = zipWith (\url result -> case result of
        Left _err -> []
        Right cont -> parseFeed url cont
        ) urls feeds
  
  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)
  
  let html = renderHtml allEntries
  TIO.writeFile "index.html" html
  putStrLn "Generated index.html"
