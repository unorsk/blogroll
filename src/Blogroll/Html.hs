{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Html where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchAllFavicons, fetchFeed)
import Blogroll.Type (FeedEntry (..), Blogroll (..))
import Control.Concurrent.Async (mapConcurrently)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
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


renderAll :: Blogroll -> IO ()
renderAll blogroll = do
  let urls = blogroll.urls
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
  let recentHtml = renderHtml recent25 blogroll.title faviconCss
  let allHtml = renderHtml allEntries (blogroll.title <> " - All Posts") faviconCss

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn "Generated index.html (25 recent) and all.html"

renderHtml :: [FeedEntry] -> Text -> Text -> Text
renderHtml entries title faviconCss =
  let entriesHtml = T.concat $ map renderEntry entries
   in """
      <!DOCTYPE html>
      <html>
      <head>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>RSS Reader</title>
      <style>
           body {
             font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
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
