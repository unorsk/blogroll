{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Fetch (fetchFeed, fetchFavicon, loadFontAsBase64, extractDomain) where

import Blogroll.Type (Warning (..))
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (diffUTCTime, getCurrentTime)
import Network.HTTP.Simple (HttpException, getResponseBody, httpLBS, parseRequest, setRequestHeaders)
import Network.URI (URI (..), uriRegName)

loadFontAsBase64 :: FilePath -> IO (Either Warning Text)
loadFontAsBase64 fontPath = do
  result <- try $ do
    fontBytes <- BS.readFile fontPath
    return $ TE.decodeUtf8 $ Base64.encode fontBytes
  case result of
    Left (e :: IOException) -> return $ Left $ FontLoadFailed fontPath (show e)
    Right base64 -> return $ Right base64

fetchFeed :: URI -> IO (Either Warning L8.ByteString)
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
    Left (e :: HttpException) -> return $ Left $ FetchFailed url (show e)
    Right body -> return $ Right body

extractDomain :: URI -> Maybe Text
extractDomain url = T.pack . uriRegName <$> uriAuthority url

fetchFavicon :: Text -> IO (Either Warning Text)
fetchFavicon domain = do
  let faviconUrl = "https://www.google.com/s2/favicons?domain=" <> domain <> "&sz=128"
  result <- try $ do
    request <- parseRequest (T.unpack faviconUrl)
    response <- httpLBS request
    let imageBytes = getResponseBody response
    return $ TE.decodeUtf8 $ Base64.encode $ L8.toStrict imageBytes
  case result of
    Left (e :: HttpException) -> return $ Left $ FaviconFetchFailed domain (show e)
    Right base64 -> return $ Right base64
