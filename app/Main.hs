{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchFavicon, fetchFeed, loadFontAsBase64)
import Blogroll.Html (generateFaviconCss, renderHtml)
import Blogroll.Type (Blogroll (..))
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.URI (URI, parseURI)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [blogrollPath, blogrollName, pathToFontFile] -> do
      urls <- readUrlsFromFile (T.pack blogrollPath)
      let blogroll = Blogroll {title = T.pack blogrollName, pathToFontFile = pathToFontFile, urls = urls}
      putStrLn $ "Found " ++ show (length urls) ++ " feeds"
      generateBlogroll blogroll
      where
        readUrlsFromFile :: T.Text -> IO [URI]
        readUrlsFromFile path = do
          input <- TIO.readFile (T.unpack path)
          return $ mapMaybe (parseURI . T.unpack) $ filter (not . T.null) $ map T.strip $ T.lines input
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <blogroll-file> <blogroll-title> <font-path>"

generateBlogroll :: Blogroll -> IO ()
generateBlogroll blogroll = do
  let urls = blogroll.urls
  fontBase64 <- loadFontAsBase64 blogroll.pathToFontFile

  results <- mapConcurrently fetchUrlData urls

  let faviconCss = generateFaviconCss [(extractDomain url, base64) | (url, (Just base64, _)) <- zip urls results]

  let feedEntries =
        [ case feedResult of
            Left _err -> []
            Right cont -> parseFeed url cont
        | (url, (_, feedResult)) <- zip urls results
        ]

  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)

  let recent25 = take 25 allEntries
  let recentHtml = renderHtml recent25 blogroll.title faviconCss fontBase64
  let allHtml = renderHtml allEntries (blogroll.title <> " - All Posts") faviconCss fontBase64

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn "Generated index.html (25 recent) and all.html"
  where
    fetchUrlData url = do
      let domain = extractDomain url
      concurrently (fetchFavicon domain) (fetchFeed url)
