{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Html where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchAllFavicons, fetchFeed)
import Blogroll.Type (FeedEntry (..), OpmlFeed (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime)

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

loadFontAsBase64 :: FilePath -> IO (Maybe Text)
loadFontAsBase64 fontPath = do
  result <- try $ do
    fontBytes <- BS.readFile fontPath
    let base64Text = TE.decodeUtf8 $ Base64.encode fontBytes
    return base64Text
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right base64 -> return $ Just base64

renderAll :: [Text] -> OpmlFeed -> IO ()
renderAll urls opmlFeed = do
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
          generateDomainCssClass (extractDomain entry.entrySiteUrl),
          "\">",
          entryTitle entry,
          "</a><span class=\"source\">(",
          extractDomain entry.entrySiteUrl,
          ")</span></div>",
          "<div class=\"date\">",
          T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" entry.entryDate,
          "</div></li>"
        ]