{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Opml where

import Blogroll.Fetch (fetchFeed)
import Blogroll.Type (FeedType (..), OpmlFeed (..), OpmlFeedEntry (..))
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Text.XML (Element (elementName), Name (..), Node (NodeElement), def, parseLBS)
import Text.XML.Cursor

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

parseOpmlEntries :: Cursor -> [OpmlFeedEntry]
parseOpmlEntries cursor = do
  outline <- cursor $// element "outline"
  let typeAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "type"
      textAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "text"
      titleAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "title"
      urlAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "xmlUrl"
  if typeAttr == "rss" && not (T.null urlAttr)
    then [OpmlFeedEntry RSS textAttr titleAttr urlAttr]
    else []

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