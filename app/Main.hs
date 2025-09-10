{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Error
import Blogroll.Html (renderAll)
import Blogroll.Opml (fetchBlogrollOpml, fetchFeedInfo, generateOpmlXml)
import Blogroll.Type (OpmlFeed (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--generate-opml"] -> do
      urls <- readUrlsFromStdin
      putStrLn $ "Fetching info for " ++ show (length urls) ++ " feeds"
      results <- mapConcurrently (runBlogrollM . fetchFeedInfo) urls
      let (errors, validFeeds) = partitionEithers results
          urlErrorPairs = zip urls results
          failedUrls = [url | (url, Left _) <- urlErrorPairs]
      putStrLn $ "Successfully fetched " ++ show (length validFeeds) ++ " feeds"
      forM_ (zip failedUrls errors) $ \(url, err) -> do
        TIO.hPutStrLn stderr $ "Failed to fetch " <> url <> ": " <> T.pack (show err)
      opmlXml <- generateOpmlXml validFeeds
      TIO.putStrLn opmlXml
    [opmlPath] -> do
      result <- runBlogrollM $ fetchBlogrollOpml (T.pack opmlPath)
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
          exitFailure
        Right opmlFeed -> do
          putStrLn $ "Found " ++ show (length opmlFeed.entries) ++ " feeds"
          renderResult <- runBlogrollM $ renderAll opmlFeed
          case renderResult of
            Left err ->
              TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
            Right _ -> return ()
          return ()
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <opml-file-or-url>    Generate HTML from OPML file or URL"
      putStrLn "  blogroll --generate-opml       Generate OPML from URLs on stdin"
  where
    readUrlsFromStdin :: IO [Text]
    readUrlsFromStdin = do
      input <- TIO.getContents
      return $ filter (not . T.null) $ map T.strip $ T.lines input