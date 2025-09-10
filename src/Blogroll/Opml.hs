{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Opml where

import Blogroll.Error
import Blogroll.Fetch (fetchFeed)
import Blogroll.Type (FeedType (..), OpmlFeed (..), OpmlFeedEntry (..))
import Control.Monad (unless)
import Control.Monad.Except ()
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Text.XML (Element (elementName), Name (..), Node (NodeElement), def, parseLBS)
import Text.XML.Cursor

readBlogrollOpml :: FilePath -> BlogrollM OpmlFeed
readBlogrollOpml path = do
  result <- liftIO $ tryIO $ L8.readFile path
  case result of
    Left ex -> throwBlogrollError $ FileError path (T.pack $ show ex)
    Right xmlContent -> parseOpmlDocument xmlContent

fetchBlogrollOpml :: Text -> BlogrollM OpmlFeed
fetchBlogrollOpml urlOrPath = do
  if "http://" `T.isPrefixOf` urlOrPath || "https://" `T.isPrefixOf` urlOrPath
    then do
      unless (isValidUrl urlOrPath) $
        throwBlogrollError $
          ValidationError $
            "Invalid URL: " <> urlOrPath
      result <- fetchFeed urlOrPath
      case result of
        (200, xmlContent) -> parseOpmlDocument xmlContent
        _ -> throwBlogrollError $ FetchError urlOrPath "Failed to fetch"
    else do
      -- It's a local file path
      readBlogrollOpml (T.unpack urlOrPath)

parseOpmlDocument :: L8.ByteString -> BlogrollM OpmlFeed
parseOpmlDocument xmlContent = do
  case parseLBS def xmlContent of
    Left err -> throwBlogrollError $ ParseError $ "Failed to parse OPML XML: " <> T.pack (show err)
    Right doc -> do
      let cursor = fromDocument doc
          titleText = T.concat $ cursor $// element "title" &// content
          ownerNameText = T.concat $ cursor $// element "ownerName" &// content
          ownerEmailText = T.concat $ cursor $// element "ownerEmail" &// content
          entries = parseOpmlEntries cursor
      return $ OpmlFeed titleText ownerNameText ownerEmailText entries

parseOpmlEntries :: Cursor -> [OpmlFeedEntry]
parseOpmlEntries cursor = do
  outline <- cursor $// element "outline"
  let typeAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "type"
      textAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "text"
      titleAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "title"
      urlAttr = fromMaybe "" $ listToMaybe $ outline $| attribute "xmlUrl"
  if typeAttr == "rss" && not (T.null urlAttr)
    then [OpmlFeedEntry RSS textAttr titleAttr urlAttr]
    else [] -- TODO error handling

generateOpmlXml :: [OpmlFeedEntry] -> IO Text
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
    -- everything is 'rss' according to the opml spec
    generateOutline opmlFeedEntry =
      T.concat
        ["    <outline type=\"", "rss", "\" text=\"", opmlFeedEntry.text, "\" title=\"", opmlFeedEntry.title, "\" xmlUrl=\"", opmlFeedEntry.url, "\" />\n"]

fetchFeedInfo :: Text -> BlogrollM OpmlFeedEntry
fetchFeedInfo url = do
  unless (isValidUrl url) $
    throwBlogrollError $
      ValidationError $
        "Invalid URL: " <> url
  result <- fetchFeed url
  case result of
    (200, xmlContent) -> do
      case parseLBS def xmlContent of
        Left err -> throwBlogrollError $ ParseError $ "Failed to parse feed XML: " <> T.pack (show err)
        Right doc -> do
          let cursor = fromDocument doc

          -- Try RSS parsing first
          case tryRssParsing cursor of
            Just (title, description) -> return (OpmlFeedEntry RSS title description url)
            Nothing ->
              -- If RSS fails, try Atom parsing
              case tryAtomParsing cursor of
                Just (title, description) -> return (OpmlFeedEntry ATOM title description url)
                Nothing -> throwBlogrollError $ ParseError $ "Could not parse as RSS or Atom feed: " <> url
    (_, _xmlContent) -> throwBlogrollError $ FetchError url "Failed to fetch"

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
            [ atomContent
            | c <- cursor $// anyElement,
              case node c of
                NodeElement e -> elementName e == Name "title" (Just "http://www.w3.org/2005/Atom") Nothing
                _ -> False,
              atomContent <- c $// content
            ]
          atomTitle = T.concat $ take 1 atomTitles
          atomSubtitle = "" -- Atom feeds often don't have subtitles
      if T.null atomTitle
        then Nothing
        else Just (atomTitle, atomSubtitle)
    else Nothing