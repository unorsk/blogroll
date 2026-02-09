{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Html where

import Blogroll.Fetch (extractDomain)
import Blogroll.Type (FeedEntry (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (defaultTimeLocale, formatTime)
import Lucid

generateDomainCssClass :: Text -> Text
generateDomainCssClass domain =
  "favicon-" <> T.map (\c -> if c `elem` ['.', '-'] then '_' else c) domain

generateFaviconCss :: [(Text, Text)] -> Text
generateFaviconCss favicons =
  T.concat $ map generateSingleCss favicons
  where
    generateSingleCss (domain, base64) =
      T.concat
        [ ".",
          generateDomainCssClass domain,
          "::after {",
          "content: ''; ",
          "display: inline-block; ",
          "width: 16px; ",
          "height: 16px; ",
          "margin-left: 8px; ",
          "background-image: url(data:image/png;base64,",
          base64,
          "); ",
          "background-size: contain; ",
          "background-repeat: no-repeat; ",
          "vertical-align: middle; ",
          "}\n"
        ]

renderHtml :: [FeedEntry] -> Text -> Text -> Maybe Text -> Text
renderHtml entries pageTitle faviconCss maybeFontBase64 =
  let css = generateStyles maybeFontBase64 <> faviconCss
   in TL.toStrict $
        renderText $
          doctype_
            <> html_
              ( do
                  head_
                    ( do
                        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                        title_ "RSS Reader"
                        style_ [] (toHtmlRaw css)
                    )
                  body_
                    ( do
                        h1_ (toHtml pageTitle)
                        ul_
                          ( do
                              mapM_ renderEntry entries
                              li_ (a_ [href_ "all.html"] "See all")
                          )
                    )
              )
  where
    generateStyles :: Maybe Text -> Text
    generateStyles maybeFontB64 =
      let fontFace = case maybeFontB64 of
            Just fontBase64 ->
              """@font-face {
              font-family: 'A Very Nice Font';
              src: url(data:font/woff2;base64,"""
                <> fontBase64
                <> """) format('woff2');
                     font-weight: 400;
                   }"""
            Nothing -> "" -- we don't have font, so no style here
       in fontFace
            <> """
               body {
                 font-family: 'A Very Nice Font', Helvetica, Arial, system-ui, -apple-system, sans-serif;
                 font-weigth: 400;
                 max-width: 800px;
                 margin: 0 auto;
                 color: #333;
                 display: flex;
                 flex-flow: column;
                 align-content: center;
               }
               h1 {
                 color: #2c3e50;
                 border-bottom: 2px solid #3498db;
                 padding-bottom: 10px;
               }
               ul {
                 list-style: none;
                 padding: 0;
                 margin-top: 0
               }
               li {
                 padding-left: 8px;
                 padding-bottom: 8px;
               }
               a {
                 color: #2980b9;
                 text-decoration: none;
                 font-weight: 500;
               }
               a:hover {
                 text-decoration: underline;
               }
               .date {
                 color: #7f8c8d;
                 font-size: 0.6em;
               }
               .source {
                 color: #95a5a6;
                 font-size: 0.8em;
                 padding-left: 0.5em;
               }
               """

    renderEntry :: FeedEntry -> Html ()
    renderEntry entry =
      li_
        ( do
            div_
              ( do
                  a_
                    [ href_ (T.pack $ show $ entryLink entry),
                      class_ (generateDomainCssClass (extractDomain entry.entrySiteUrl))
                    ]
                    (toHtml $ entryTitle entry)
                  span_ [class_ "source"] (toHtml $ "(" <> extractDomain entry.entrySiteUrl <> ")")
              )
            div_ [class_ "date"] (toHtml $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" entry.entryDate)
        )
