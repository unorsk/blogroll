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
import Options.Applicative

data Options = Options
  { optBlogrollPath :: FilePath,
    optTitle :: Maybe T.Text,
    optFontPath :: Maybe FilePath,
    optRecentCount :: Int
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "BLOGROLL_FILE" <> help "Path to file with feed URLs")
    <*> optional (option (T.pack <$> str) (long "title" <> short 't' <> metavar "TITLE" <> help "Blogroll title"))
    <*> optional (strOption (long "font" <> short 'f' <> metavar "FONT_PATH" <> help "Path to .woff2 font file"))
    <*> option auto (long "recent" <> short 'n' <> metavar "N" <> value 25 <> showDefault <> help "Number of recent entries on the front page")

main :: IO ()
main = do
  opts <- execParser parserInfo
  urls <- readUrlsFromFile opts.optBlogrollPath
  let blogroll =
        Blogroll
          { title = maybe "Blogroll" id opts.optTitle,
            pathToFontFile = opts.optFontPath,
            recentCount = opts.optRecentCount,
            urls = urls
          }
  putStrLn $ "Found " ++ show (length urls) ++ " feeds"
  generateBlogroll blogroll
  where
    parserInfo =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Generate an HTML blogroll from RSS/Atom feed URLs")

readUrlsFromFile :: FilePath -> IO [URI]
readUrlsFromFile path = do
  input <- TIO.readFile path
  return $ mapMaybe (parseURI . T.unpack) $ filter (not . T.null) $ map T.strip $ T.lines input

generateBlogroll :: Blogroll -> IO ()
generateBlogroll blogroll = do
  let urls = blogroll.urls
  fontBase64 <- case blogroll.pathToFontFile of
    Just path -> loadFontAsBase64 path
    Nothing -> return Nothing

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

  let recent = take blogroll.recentCount allEntries
  let recentHtml = renderHtml recent blogroll.title faviconCss fontBase64
  let allHtml = renderHtml allEntries (blogroll.title <> " - All Posts") faviconCss fontBase64

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn $ "Generated index.html (" ++ show blogroll.recentCount ++ " recent) and all.html"
  where
    fetchUrlData url = do
      let domain = extractDomain url
      concurrently (fetchFavicon domain) (fetchFeed url)
