{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Html (renderAll)
import Blogroll.Opml (fetchBlogrollOpml, fetchFeedInfo, generateOpmlXml)
import Blogroll.Type (OmplFeedEntry (..), OpmlFeed (..))
import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

readUrlsFromStdin :: IO [Text]
readUrlsFromStdin = do
  input <- TIO.getContents
  return $ filter (not . T.null) $ map T.strip $ T.lines input

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

      renderAll urls opmlFeed
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <opml-file-or-url>    Generate HTML from OPML file or URL"
      putStrLn "  blogroll --generate-opml       Generate OPML from URLs on stdin"
