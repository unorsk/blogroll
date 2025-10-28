{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Html where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchAllFavicons, fetchFeed)
import Blogroll.Type (Blogroll (..), FeedEntry (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Time (defaultTimeLocale, formatTime)
import Lucid

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

-- TODO split this one into fetching and rendering
renderAll :: Blogroll -> IO ()
renderAll blogroll = do
  let urls = blogroll.urls
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
  let recentHtml = renderHtml recent25 blogroll.title faviconCss fontBase64
  let allHtml = renderHtml allEntries (blogroll.title <> " - All Posts") faviconCss fontBase64

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn "Generated index.html (25 recent) and all.html"

renderHtml :: [FeedEntry] -> Text -> Text -> Maybe Text -> Text
renderHtml entries pageTitle faviconCss maybeFontBase64 =
  TL.toStrict $ renderText $ doctype_ <> html_ (do
    head_ (do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ "RSS Reader"
      style_ [] (toHtmlRaw $ generateStyles faviconCss maybeFontBase64))
    body_ (do
      h1_ (toHtml pageTitle)
      ul_ (do
        mapM_ renderEntry entries
        li_ (a_ [href_ "all.html"] "See all"))))
  where
    generateStyles :: Text -> Maybe Text -> Text
    generateStyles css maybeFontB64 =
      let fontFace = case maybeFontB64 of
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
       in fontFace
            <> """
           body {
             font-family: 'IBM Plex Sans', -apple-system, sans-serif;
             max-width: 800px;
             margin: 0 auto;
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
            <> css

    renderEntry :: FeedEntry -> Html ()
    renderEntry entry =
      li_ (do
        div_ (do
          a_
            [ href_ (T.pack $ show $ entryLink entry),
              class_ (generateDomainCssClass (extractDomain entry.entrySiteUrl))
            ]
            (toHtml $ entryTitle entry)
          span_ [class_ "source"] (toHtml $ "(" <> extractDomain entry.entrySiteUrl <> ")"))
        div_ [class_ "date"] (toHtml $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" entry.entryDate))