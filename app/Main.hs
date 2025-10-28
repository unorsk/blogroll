{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Html (renderAll)
import Blogroll.Type (Blogroll (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.URI (URI, parseURI)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [blogrollPath] -> do
      urls <- readUrlsFromFile (T.pack blogrollPath)
      let blogroll = Blogroll {title = "Good Stuff!", urls = urls}
      putStrLn $ "Found " ++ show (length urls) ++ " feeds"
      renderAll blogroll
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <blogroll-file>    Generate HTML from blogroll file with URLs"
  where
    readUrlsFromFile :: Text -> IO [URI]
    readUrlsFromFile path = do
      input <- TIO.readFile (T.unpack path)
      return $ mapMaybe (parseURI . T.unpack) $ filter (not . T.null) $ map T.strip $ T.lines input