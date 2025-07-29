{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
module Main where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding qualified as TE
import Data.Time
import Network.HTTP.Simple
import Text.XML as XML
import Text.XML.Cursor
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.List (sortBy, nub)
import Data.Ord (Down(..), comparing)
import Control.Exception (try, SomeException)
import Control.Applicative ((<|>))
import qualified Data.Map as Map

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
  start <- getCurrentTime
  result <- try $ do
    request <- parseRequest (T.unpack url)
    let requestWithHeaders = setRequestHeaders
          [ ("User-Agent", "Blogroll RSS Reader/1.0")
          , ("Accept", "application/rss+xml, application/xml, text/xml")
          ] request
    response <- httpLBS requestWithHeaders
    return $ getResponseBody response
  end <- getCurrentTime
  let duration = diffUTCTime end start
  putStrLn $ "Fetched " ++ T.unpack url ++ " in " ++ show duration
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
  case parseRssTime (T.unpack pubDateStr) of
    Just date -> [FeedEntry title link date siteUrl]
    Nothing -> []

parseRssTime :: String -> Maybe UTCTime
parseRssTime dateStr =
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" dateStr <|>
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" dateStr

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
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" dateStr <|>
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateStr <|>
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dateStr

mergeFeedEntries :: [[FeedEntry]] -> [FeedEntry]
mergeFeedEntries = sortBy (comparing (Down . entryDate)) . concat

renderHtml :: [FeedEntry] -> Text -> Text -> Maybe Text -> Text
renderHtml entries title faviconCss maybeFontBase64 =
  let entriesHtml = T.concat $ map renderEntry entries
      fontFace = case maybeFontBase64 of
        Just fontBase64 -> """@font-face {
  font-family: 'IBM Plex Sans';
  src: url(data:font/woff2;base64,""" <> fontBase64 <> """) format('woff2');
  font-weight: 400;
}"""
        Nothing -> """@font-face {
  font-family: 'IBM Plex Sans';
  src: url('IBMPlexSans-Regular.woff2') format('woff2');
  font-weight: 400;
}"""
  in """
<!DOCTYPE html>
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>RSS Reader</title>
<style>
""" <> fontFace <> """
body {
  font-family: 'IBM Plex Sans', -apple-system, sans-serif;
  max-width: 800px;
  margin: 0 auto;
  /* padding-left: 0.5em; */
  /* padding-right: 0.5em; */
  color: #333;
  display: flex;
  flex-flow: column;
  align-content: center;
}
h1 {
  color: #2c3e50;
  border-bottom: 2px solid #3498db;
  padding-bottom: 10px;
}
ul {
  list-style: none;
  padding: 0;
  margin-top: 0
}
li {
  padding-left: 8px;
  padding-bottom: 8px;
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
  font-size: 0.6em;
}
.source {
  color: #95a5a6;
  font-size: 0.8em;
  padding-left: 0.5em;
}
""" <> faviconCss <> """
</style>
</head>
 <body>
  <h1>""" <> title <> """</h1>
   <ul>
    """ <> entriesHtml <> """
   <li><a href=\"all.html\">See all</a></li>
   </ul>
 </body>
</html>
"""
  where
    renderEntry entry = T.concat
      [ "<li><div><a href=\"", entryLink entry, "\" class=\"", generateDomainCssClass (extractDomain (entrySiteUrl entry)), "\">"
      , entryTitle entry, "</a><span class=\"source\">(", extractDomain (entrySiteUrl entry), ")</span></div>"
      , "<div class=\"date\">", T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (entryDate entry), "</div></li>"
      ]

extractDomain :: Text -> Text
extractDomain url =
  let withoutProtocol = T.drop (T.length "https://") url
  in T.takeWhile (/= '/') withoutProtocol

fetchFavicon :: Text -> IO (Maybe Text)
fetchFavicon domain = do
  let faviconUrl = "https://www.google.com/s2/favicons?domain=" <> domain <> "&sz=128"
  result <- try $ do
    request <- parseRequest (T.unpack faviconUrl)
    response <- httpLBS request
    let imageBytes = getResponseBody response
    let base64Text = TE.decodeUtf8 $ Base64.encode $ L8.toStrict imageBytes
    return base64Text
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right base64 -> return $ Just base64

generateDomainCssClass :: Text -> Text
generateDomainCssClass domain =
  "favicon-" <> T.map (\c -> if c `elem` ['.','-'] then '_' else c) domain

generateFaviconCss :: Map.Map Text Text -> Text
generateFaviconCss faviconMap =
  T.concat $ map generateSingleCss (Map.toList faviconMap)
  where
    generateSingleCss (domain, base64) = T.concat
      [ ".", generateDomainCssClass domain, "::after {"
      , "content: ''; "
      , "display: inline-block; "
      , "width: 16px; "
      , "height: 16px; "
      , "margin-left: 8px; "
      , "background-image: url(data:image/png;base64,", base64, "); "
      , "background-size: contain; "
      , "background-repeat: no-repeat; "
      , "vertical-align: middle; "
      , "}\n"
      ]

fetchAllFavicons :: [Text] -> IO (Map.Map Text Text)
fetchAllFavicons urls = do
  let domains = nub $ map extractDomain urls
  putStrLn $ "Fetching favicons for " ++ show (length domains) ++ " domains"
  faviconResults <- mapM (\domain -> do
    favicon <- fetchFavicon domain
    return (domain, favicon)
    ) domains
  return $ Map.fromList $ mapMaybe (\(domain, maybeFavicon) ->
    case maybeFavicon of
      Just favicon -> Just (domain, favicon)
      Nothing -> Nothing
    ) faviconResults

loadFontAsBase64 :: FilePath -> IO (Maybe Text)
loadFontAsBase64 fontPath = do
  result <- try $ do
    fontBytes <- BS.readFile fontPath
    let base64Text = TE.decodeUtf8 $ Base64.encode fontBytes
    return base64Text
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right base64 -> return $ Just base64

main :: IO ()
main = do
  urls <- readBlogroll "blogroll.txt"
  putStrLn $ "Found " ++ show (length urls) ++ " feeds"

  fontBase64 <- loadFontAsBase64 "IBMPlexSans-Regular.woff2"
  faviconMap <- fetchAllFavicons urls
  let faviconCss = generateFaviconCss faviconMap

  feeds <- mapM fetchFeed urls
  let feedEntries = zipWith (\url result -> case result of
        Left _err -> []
        Right cont -> parseFeed url cont
        ) urls feeds

  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)

  let recent25 = take 25 allEntries
  let recentHtml = renderHtml recent25 "Good stuff!" faviconCss fontBase64
  let allHtml = renderHtml allEntries "All Posts" faviconCss fontBase64

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn "Generated index.html (25 recent) and all.html"
