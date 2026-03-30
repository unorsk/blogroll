{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchFavicon, fetchFeed, loadFontAsBase64)
import Blogroll.Html (generateFaviconCss, renderHtml)
import Blogroll.Type (Blogroll (..), PageKind (..), RenderConfig (..), Warning (..), formatWarning)
import Control.Concurrent.Async (concurrently, mapConcurrently)

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
    <*> optional (strOption (long "font" <> short 'f' <> metavar "FONT_PATH" <> help "Path to .woff2 font file"))
    <*> option auto (long "recent" <> short 'n' <> metavar "N" <> value 25 <> showDefault <> help "Number of recent entries on the front page")

main :: IO ()
main = do
  opts <- execParser parserInfo
  (urls, urlWarnings) <- readUrlsFromFile opts.optBlogrollPath
  mapM_ (hPutStrLn stderr . formatWarning) urlWarnings
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

  (fontResult, results) <- concurrently
    (case blogroll.pathToFontFile of
      Just path -> Just <$> loadFontAsBase64 path
      Nothing -> return Nothing)
    (mapConcurrently fetchUrlData urls)

  let (fontBase64, fontWarnings) = case fontResult of
        Nothing -> (Nothing, [])
        Just (Left w) -> (Nothing, [w])
        Just (Right b64) -> (Just b64, [])

  let (faviconResults, feedResults) = unzip results
  let (faviconWarnings, favicons) = foldr classifyFavicon ([], []) (zip urls faviconResults)
  let faviconCss = generateFaviconCss favicons

  let (feedEntries, feedWarnings) = unzip
        [ case feedResult of
            Left w -> ([], [w])
            Right cont -> parseFeed url cont
        | (url, feedResult) <- zip urls feedResults
        ]

  let allWarnings = fontWarnings ++ faviconWarnings ++ concat feedWarnings
  mapM_ (hPutStrLn stderr . formatWarning) allWarnings

  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)

  let recent = take blogroll.recentCount allEntries
  let recentConfig = RenderConfig
        { pageKind = RecentPage
        , pageTitle = blogroll.title
        , faviconCss = faviconCss
        , fontBase64 = fontBase64
        }
  let allConfig = recentConfig
        { pageKind = AllPostsPage
        , pageTitle = blogroll.title <> " - All Posts"
        }
  let recentHtml = renderHtml recent recentConfig
  let allHtml = renderHtml allEntries allConfig

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn $ "Generated index.html (" ++ show blogroll.recentCount ++ " recent) and all.html"
  where
    classifyFavicon (url, favResult) (ws, fs) =
      case (extractDomain url, favResult) of
        (Just domain, Right base64) -> (ws, (domain, base64) : fs)
        (_, Left w) -> (w : ws, fs)
        _ -> (ws, fs) -- no domain, silently skip

    fetchUrlData url = do
      let fetchFav = case extractDomain url of
            Just domain -> fetchFavicon domain
            Nothing -> return $ Left $ FaviconFetchFailed "" "no domain"
      concurrently fetchFav (fetchFeed url)
