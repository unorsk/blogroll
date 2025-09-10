{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Error 
  ( BlogrollError(..)
  , BlogrollM
  , runBlogrollM
  , throwBlogrollError
  , catchBlogrollError
  , liftBlogrollEither
  , isValidUrl
  , tryIO
  ) where

import Control.Monad.Except
import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T

-- Main error ADT covering all failure modes
data BlogrollError 
  = ParseError Text           -- XML/OPML parsing failures
  | FetchError Text Text       -- URL + error message
  | FileError FilePath Text    -- File path + error message
  | ValidationError Text       -- Input validation failures
  | TimeParseError Text        -- Date/time parsing failures
  deriving (Show, Eq)

-- Main monad transformer stack
type BlogrollM = ExceptT BlogrollError IO

-- Helper functions
runBlogrollM :: BlogrollM a -> IO (Either BlogrollError a)
runBlogrollM = runExceptT

throwBlogrollError :: BlogrollError -> BlogrollM a
throwBlogrollError = throwError

catchBlogrollError :: BlogrollM a -> (BlogrollError -> BlogrollM a) -> BlogrollM a
catchBlogrollError = catchError

liftBlogrollEither :: Either BlogrollError a -> BlogrollM a
liftBlogrollEither = either throwError return

-- URL validation helper
isValidUrl :: Text -> Bool
isValidUrl url = 
  ("http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url) &&
  T.length url > 10 &&
  not (T.null domain)
  where
    cleaned = if "https://" `T.isPrefixOf` url 
              then T.drop 8 url
              else if "http://" `T.isPrefixOf` url
                   then T.drop 7 url
                   else url
    domain = T.takeWhile (/= '/') cleaned

-- IO exception helper
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try