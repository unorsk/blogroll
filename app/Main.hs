{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchFavicon, fetchFeed, loadFontAsBase64)
import Blogroll.Html (generateFaviconCss, renderHtml)
import Blogroll.Type (Blogroll (..), PageKind (..), RenderConfig (..), Warning (..), formatWarning)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.URI (URI, parseURI, uriScheme)
import Options.Applicative
import System.IO (hPutStrLn, stderr)

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
    <*> optional (strOption (long "font" <> short 'f' <> metavar "FONT_PATH" <> help "Path to a .woff2 font file (defaults to the system sans-serif font)"))
    <*> option auto (long "recent" <> short 'n' <> metavar "N" <> value 25 <> showDefault <> help "Number of recent entries on the front page")

main :: IO ()
main = do
  opts <- execParser parserInfo
  (urls, urlWarnings) <- readUrlsFromFile opts.optBlogrollPath
  mapM_ (hPutStrLn stderr . formatWarning) urlWarnings
  let blogroll =
        Blogroll
          { title = fromMaybe "Blogroll" opts.optTitle,
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

readUrlsFromFile :: FilePath -> IO ([URI], [Warning])
readUrlsFromFile path = do
  input <- TIO.readFile path
  let nonEmpty = filter (not . T.null) $ map T.strip $ T.lines input
  return $ foldr classifyLine ([], []) nonEmpty
  where
    classifyLine line (urls, warnings) =
      case parseURI (T.unpack line) of
        Just uri
          | uriScheme uri `elem` ["http:", "https:"] -> (uri : urls, warnings)
          | otherwise -> (urls, InvalidUrl line : warnings)
        Nothing -> (urls, InvalidUrl line : warnings)

generateBlogroll :: Blogroll -> IO ()
generateBlogroll blogroll = do
  let urls = blogroll.urls
      domains = nub $ mapMaybe extractDomain urls

  (fontResult, (faviconResults, feedResults)) <-
    concurrently
      (traverse loadFontAsBase64 blogroll.pathToFontFile)
      ( concurrently
          (mapConcurrently fetchDomainFavicon domains)
          (mapConcurrently fetchFeed urls)
      )

  let (fontBase64, fontWarnings) = case fontResult of
        Nothing -> (Nothing, [])
        Just (Left w) -> (Nothing, [w])
        Just (Right b64) -> (Just b64, [])

  let (faviconWarnings, favicons) = partitionEithers faviconResults
  let faviconCss = generateFaviconCss favicons

  let (feedEntries, feedWarnings) =
        unzip
          [ either (\w -> ([], [w])) (parseFeed url) feedResult
          | (url, feedResult) <- zip urls feedResults
          ]

  let allWarnings = fontWarnings ++ faviconWarnings ++ concat feedWarnings
  mapM_ (hPutStrLn stderr . formatWarning) allWarnings

  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)

  let recent = take blogroll.recentCount allEntries
  let recentConfig =
        RenderConfig
          { pageKind = RecentPage,
            pageTitle = blogroll.title,
            faviconCss = faviconCss,
            fontBase64 = fontBase64
          }
  let allConfig =
        recentConfig
          { pageKind = AllPostsPage,
            pageTitle = blogroll.title <> " - All Posts"
          }

  TIO.writeFile "index.html" (renderHtml recent recentConfig)
  TIO.writeFile "all.html" (renderHtml allEntries allConfig)
  putStrLn $ "Generated index.html (" ++ show blogroll.recentCount ++ " recent) and all.html"
  where
    fetchDomainFavicon domain = fmap (\b64 -> (domain, b64)) <$> fetchFavicon domain
