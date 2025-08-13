{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (nub, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time
import Network.HTTP.Simple
import System.Environment (getArgs)
import Text.XML as XML
import Text.XML.Cursor

data FeedType = RSS | ATOM
  deriving (Show, Eq)

data OpmlFeed = OpmlFeed
  { title :: Text,
    -- <dateModified>Mon, 11 Aug 2025 08:01:37 GMT</dateModified>
    ownerName :: Text,
    ownerEmail :: Text,
    entries :: [OmplFeedEntry]
  }
  deriving (Show, Eq)

data OmplFeedEntry = OmplFeedEntry
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

readBlogrollOpml :: FilePath -> IO OpmlFeed
readBlogrollOpml path = do
  xmlContent <- L8.readFile path
  case parseLBS def xmlContent of
    Left err -> error $ "Failed to parse OPML: " ++ show err
    Right doc ->
      let cursor = fromDocument doc
          titleText = T.concat $ cursor $// element "title" &// content
          ownerNameText = T.concat $ cursor $// element "ownerName" &// content
          ownerEmailText = T.concat $ cursor $// element "ownerEmail" &// content
          entries = parseOpmlEntries cursor
       in return $ OpmlFeed titleText ownerNameText ownerEmailText entries

fetchBlogrollOpml :: Text -> IO OpmlFeed
fetchBlogrollOpml urlOrPath = do
  if "http://" `T.isPrefixOf` urlOrPath || "https://" `T.isPrefixOf` urlOrPath
    then do
      -- It's a URL, fetch it
      result <- fetchFeed urlOrPath
      case result of
        Left err -> error $ "Failed to fetch OPML from URL: " ++ err
        Right xmlContent -> parseOpmlContent xmlContent
    else do
      -- It's a local file path
      readBlogrollOpml (T.unpack urlOrPath)
  where
    parseOpmlContent xmlContent = 
      case parseLBS def xmlContent of
        Left err -> error $ "Failed to parse OPML: " ++ show err
        Right doc ->
          let cursor = fromDocument doc
              titleText = T.concat $ cursor $// element "title" &// content
              ownerNameText = T.concat $ cursor $// element "ownerName" &// content
              ownerEmailText = T.concat $ cursor $// element "ownerEmail" &// content
              entries = parseOpmlEntries cursor
           in return $ OpmlFeed titleText ownerNameText ownerEmailText entries

parseOpmlEntries :: Cursor -> [OmplFeedEntry]
parseOpmlEntries cursor = do
  outline <- cursor $// element "outline"
  let typeAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "type"
      textAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "text"
      titleAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "title"
      urlAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "xmlUrl"
  if typeAttr == "rss" && not (T.null urlAttr)
    then [OmplFeedEntry RSS textAttr titleAttr urlAttr]
    else []

readBlogroll :: FilePath -> IO [Text]
readBlogroll path = do
  cont <- TIO.readFile path
  return $ filter (not . T.null) $ map T.strip $ T.lines cont

fetchFeed :: Text -> IO (Either String L8.ByteString)
fetchFeed url = do
  start <- getCurrentTime
  result <- try $ do
    request <- parseRequest (T.unpack url)
    let requestWithHeaders =
          setRequestHeaders
            [ ("User-Agent", "Blogroll RSS Reader/1.0"),
              ("Accept", "application/rss+xml, application/xml, text/xml")
            ]
            request
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

renderHtml :: [FeedEntry] -> Text -> Text -> Maybe Text -> Text
renderHtml entries title faviconCss maybeFontBase64 =
  let entriesHtml = T.concat $ map renderEntry entries
      fontFace = case maybeFontBase64 of
        Just fontBase64 ->
          """@font-face {
          font-family: 'IBM Plex Sans';
          src: url(data:font/woff2;base64,"""
            <> fontBase64
            <> """) format('woff2');
                 font-weight: 400;
               }"""
        Nothing ->
          """@font-face {
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
      """
        <> fontFace
        <> """
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
           """
        <> faviconCss
        <> """
           </style>
           </head>
            <body>
             <h1>"""
        <> title
        <> """</h1>
           <ul>
           """
        <> entriesHtml
        <> """
              <li><a href=\"all.html\">See all</a></li>
              </ul>
            </body>
           </html>
           """
  where
    renderEntry entry =
      T.concat
        [ "<li><div><a href=\"",
          entryLink entry,
          "\" class=\"",
          generateDomainCssClass (extractDomain (entrySiteUrl entry)),
          "\">",
          entryTitle entry,
          "</a><span class=\"source\">(",
          extractDomain (entrySiteUrl entry),
          ")</span></div>",
          "<div class=\"date\">",
          T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (entryDate entry),
          "</div></li>"
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
  "favicon-" <> T.map (\c -> if c `elem` ['.', '-'] then '_' else c) domain

generateFaviconCss :: Map.Map Text Text -> Text
generateFaviconCss faviconMap =
  T.concat $ map generateSingleCss (Map.toList faviconMap)
  where
    generateSingleCss (domain, base64) =
      T.concat
        [ ".",
          generateDomainCssClass domain,
          "::after {",
          "content: ''; ",
          "display: inline-block; ",
          "width: 16px; ",
          "height: 16px; ",
          "margin-left: 8px; ",
          "background-image: url(data:image/png;base64,",
          base64,
          "); ",
          "background-size: contain; ",
          "background-repeat: no-repeat; ",
          "vertical-align: middle; ",
          "}\n"
        ]

fetchAllFavicons :: [Text] -> IO (Map.Map Text Text)
fetchAllFavicons urls = do
  let domains = nub $ map extractDomain urls
  putStrLn $ "Fetching favicons for " ++ show (length domains) ++ " domains"
  faviconResults <-
    mapConcurrently
      ( \domain -> do
          favicon <- fetchFavicon domain
          return (domain, favicon)
      )
      domains
  return $
    Map.fromList $
      mapMaybe
        ( \(domain, maybeFavicon) ->
            case maybeFavicon of
              Just favicon -> Just (domain, favicon)
              Nothing -> Nothing
        )
        faviconResults

loadFontAsBase64 :: FilePath -> IO (Maybe Text)
loadFontAsBase64 fontPath = do
  result <- try $ do
    fontBytes <- BS.readFile fontPath
    let base64Text = TE.decodeUtf8 $ Base64.encode fontBytes
    return base64Text
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right base64 -> return $ Just base64

readUrlsFromStdin :: IO [Text]
readUrlsFromStdin = do
  input <- TIO.getContents
  return $ filter (not . T.null) $ map T.strip $ T.lines input

fetchFeedInfo :: Text -> IO (Maybe (FeedType, Text, Text, Text))
fetchFeedInfo url = do
  result <- fetchFeed url
  case result of
    Left _err -> return Nothing
    Right xmlContent -> do
      case parseLBS def xmlContent of
        Left _ -> return Nothing
        Right doc -> do
          let cursor = fromDocument doc

          -- Try RSS parsing first
          case tryRssParsing cursor of
            Just (title, description) -> return $ Just (RSS, title, description, url)
            Nothing ->
              -- If RSS fails, try Atom parsing
              case tryAtomParsing cursor of
                Just (title, description) -> return $ Just (ATOM, title, description, url)
                Nothing -> return Nothing

tryRssParsing :: Cursor -> Maybe (Text, Text)
tryRssParsing cursor = do
  let rssTitle = T.concat $ cursor $// element "channel" &/ element "title" &// content
      rssDescription = T.concat $ cursor $// element "channel" &/ element "description" &// content
  if T.null rssTitle
    then Nothing
    else Just (rssTitle, rssDescription)

tryAtomParsing :: Cursor -> Maybe (Text, Text)
tryAtomParsing cursor = do
  -- Look for Atom elements by checking if we have elements with Atom namespace
  let allElements = cursor $// anyElement
      hasAtomNamespace =
        any
          ( \c -> case node c of
              NodeElement e ->
                elementName e == Name "title" (Just "http://www.w3.org/2005/Atom") Nothing
                  || elementName e == Name "feed" (Just "http://www.w3.org/2005/Atom") Nothing
              _ -> False
          )
          allElements
  if hasAtomNamespace
    then do
      -- Extract title directly from atom title elements
      let atomTitles =
            [ content
            | c <- cursor $// anyElement,
              case node c of
                NodeElement e -> elementName e == Name "title" (Just "http://www.w3.org/2005/Atom") Nothing
                _ -> False,
              content <- c $// content
            ]
          atomTitle = T.concat $ take 1 atomTitles
          atomSubtitle = "" -- Atom feeds often don't have subtitles
      if T.null atomTitle
        then Nothing
        else Just (atomTitle, atomSubtitle)
    else Nothing

generateOpmlXml :: [(FeedType, Text, Text, Text)] -> IO Text
generateOpmlXml feedInfos = do
  now <- getCurrentTime
  let dateModified = T.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" now
      outlines = T.concat $ map generateOutline feedInfos
  return $
    T.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<opml version=\"1.0\">\n",
        "  <head>\n",
        "    <title></title>\n",
        "    <dateModified>",
        dateModified,
        "</dateModified>\n",
        "    <ownerName></ownerName>\n",
        "    <ownerEmail></ownerEmail>\n",
        "  </head>\n",
        "  <body>\n",
        outlines,
        "  </body>\n",
        "</opml>\n"
      ]
  where
    generateOutline (feedType, title, description, url) =
      let typeStr = case feedType of
            RSS -> "rss"
            ATOM -> "rss" -- OPML spec typically uses "rss" for both
       in T.concat
            ["    <outline type=\"", typeStr, "\" text=\"", description, "\" title=\"", title, "\" xmlUrl=\"", url, "\" />\n"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--generate-opml"] -> do
      urls <- readUrlsFromStdin
      putStrLn $ "Fetching info for " ++ show (length urls) ++ " feeds"
      feedInfos <- mapConcurrently fetchFeedInfo urls
      let results = zip urls feedInfos
          validFeeds = mapMaybe id feedInfos
          failedFeeds = [url | (url, Nothing) <- results]
      putStrLn $ "Successfully fetched " ++ show (length validFeeds) ++ " feeds"
      mapM_ (\url -> putStrLn $ "Failed to fetch: " ++ T.unpack url) failedFeeds
      opmlXml <- generateOpmlXml validFeeds
      TIO.putStrLn opmlXml
    [opmlPath] -> do
      opmlFeed <- fetchBlogrollOpml (T.pack opmlPath)
      putStrLn $ "Found " ++ show (length opmlFeed.entries) ++ " feeds"
      let urls = map (\entry -> entry.url) opmlFeed.entries

      fontBase64 <- loadFontAsBase64 "IBMPlexSans-Regular.woff2"
      faviconMap <- fetchAllFavicons urls
      let faviconCss = generateFaviconCss faviconMap
      feeds <- mapConcurrently fetchFeed urls
      let feedEntries =
            zipWith
              ( \url result -> case result of
                  Left _err -> []
                  Right cont -> parseFeed url cont
              )
              urls
              feeds

      let allEntries = mergeFeedEntries feedEntries
      putStrLn $ "Total entries: " ++ show (length allEntries)

      let recent25 = take 25 allEntries
      let recentHtml = renderHtml recent25 opmlFeed.title faviconCss fontBase64
      let allHtml = renderHtml allEntries (opmlFeed.title <> " - All Posts") faviconCss fontBase64

      TIO.writeFile "index.html" recentHtml
      TIO.writeFile "all.html" allHtml
      putStrLn "Generated index.html (25 recent) and all.html"
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <opml-file-or-url>    Generate HTML from OPML file or URL"
      putStrLn "  blogroll --generate-opml       Generate OPML from URLs on stdin"
