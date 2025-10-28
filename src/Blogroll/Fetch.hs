{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Fetch (fetchAllFavicons, fetchFeed, extractDomain) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeaders)
import Network.URI (URI (..), uriRegName)

fetchAllFavicons :: [URI] -> IO (Map.Map Text Text)
fetchAllFavicons urls = do
  let domains = nub $ map extractDomain urls
  putStrLn $ "Fetching favicons for " ++ show (length domains) ++ " domains"
  faviconResults <-
    mapConcurrently
      ( \domain -> do
          favicon <- fetchFavicon domain
          return (domain, favicon)
      )
      domains
  return $
    Map.fromList $
      mapMaybe
        ( \(domain, maybeFavicon) ->
            case maybeFavicon of
              Just favicon -> Just (domain, favicon)
              Nothing -> Nothing
        )
        faviconResults

fetchFeed :: URI -> IO (Either String L8.ByteString)
fetchFeed url = do
  start <- getCurrentTime
  result <- try $ do
    request <- parseRequest $ show url
    let requestWithHeaders =
          setRequestHeaders
            [ ("User-Agent", "Blogroll RSS Reader/1.0"),
              ("Accept", "application/rss+xml, application/xml, text/xml")
            ]
            request
    response <- httpLBS requestWithHeaders
    return $ getResponseBody response
  end <- getCurrentTime
  let duration = diffUTCTime end start
  putStrLn $ "Fetched " ++ show url ++ " in " ++ show duration
  case result of
    Left e -> return $ Left $ show (e :: SomeException)
    Right body -> return $ Right body

extractDomain :: URI -> Text
extractDomain url = do
  let domain1 = uriAuthority url
   in case domain1 of
        Just domain -> T.pack $ uriRegName domain
        Nothing -> T.pack "" -- TODO yet again, I'll fix this later

fetchFavicon :: Text -> IO (Maybe Text)
fetchFavicon domain = do
  let faviconUrl = "https://www.google.com/s2/favicons?domain=" <> domain <> "&sz=128"
  result <- try $ do
    request <- parseRequest (T.unpack faviconUrl)
    response <- httpLBS request
    let imageBytes = getResponseBody response
    let base64Text = TE.decodeUtf8 $ Base64.encode $ L8.toStrict imageBytes
    return base64Text
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right base64 -> return $ Just base64