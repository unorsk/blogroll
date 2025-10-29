{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blogroll.Feed (readUrlsFromFile)
import Blogroll.Html (renderAll)
import Blogroll.Type (Blogroll (..))
import Data.Text qualified as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [blogrollPath, blogrollName, pathToFontFile] -> do
      urls <- readUrlsFromFile (T.pack blogrollPath)
      let blogroll = Blogroll {title = T.pack blogrollName, pathToFontFile = pathToFontFile, urls = urls}
      putStrLn $ "Found " ++ show (length urls) ++ " feeds"
      -- TODO split this one into fetching and rendering
      renderAll blogroll
    _ -> do
      putStrLn "Usage:"
      putStrLn "  blogroll <blogroll-file> <blogroll-title> <font-path>"
