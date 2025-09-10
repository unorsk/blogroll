{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Fetch (fetchAllFavicons, fetchFeed, extractDomain) where

import Blogroll.Error (BlogrollError (FetchError), BlogrollM, runBlogrollM, throwBlogrollError)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpLBS, parseRequest, setRequestHeaders)

fetchAllFavicons :: [Text] -> BlogrollM (Map.Map Text Text)
fetchAllFavicons urls = do
  let domains = nub $ map extractDomain urls
  liftIO $ putStrLn $ "Fetching favicons for " ++ show (length domains) ++ " domains"
  faviconResults <-
    mapConcurrently
      ( runBlogrollM
          ( \domain -> do
              favicon <- fetchFavicon domain
              return (domain, favicon)
          )
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

fetchFeed :: Text -> BlogrollM (Int, L8.ByteString)
fetchFeed url = do
  start <- liftIO getCurrentTime
  result <- liftIO $ try $ do
    request <- parseRequest (T.unpack url)
    let requestWithHeaders =
          setRequestHeaders
            [ ("User-Agent", "Blogroll RSS Reader/1.0"),
              ("Accept", "application/rss+xml, application/xml, text/xml")
            ]
            request
    response <- httpLBS requestWithHeaders
    return (getResponseStatusCode response, getResponseBody response)
  end <- liftIO getCurrentTime
  let duration = diffUTCTime end start
  liftIO $ putStrLn $ "Fetched " ++ T.unpack url ++ " in " ++ show duration
  case result of
    Left e -> throwBlogrollError $ FetchError url $ T.pack (show (e :: SomeException))
    Right body -> return body

extractDomain :: Text -> Text
extractDomain url =
  let cleaned =
        fromMaybe url $
          T.stripPrefix "https://" url
            <|> T.stripPrefix "http://" url
      domain = T.takeWhile (/= '/') cleaned
   in if T.null domain then url else domain

fetchFavicon :: Text -> BlogrollM Text
fetchFavicon domain = do
  let faviconUrl = "https://www.google.com/s2/favicons?domain=" <> domain <> "&sz=128"
  do
    (code, imageBytes) <- fetchFeed faviconUrl
    case code of
      200 -> do
        -- let imageBytes = getResponseBody response
        let base64Text = TE.decodeUtf8 $ Base64.encode $ L8.toStrict imageBytes
        return base64Text
      -- case result of
      --   Left (_ :: SomeException) -> return Nothing
      --   Right base64 -> return $ Just base64
      _ -> undefined
