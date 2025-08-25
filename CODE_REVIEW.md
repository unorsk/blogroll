# Code Review - Haskell Blogroll Project

## 🔴 Critical Issues

### 1. Unsafe Error Handling
- Multiple uses of `error` for runtime failures that crash the entire program:
  - `src/Blogroll/Opml.hs:19` - Failed to parse OPML
  - `src/Blogroll/Opml.hs:35` - Failed to fetch OPML from URL
  - `src/Blogroll/Opml.hs:43` - Failed to parse OPML content
- Silent failure patterns returning empty lists/Nothing without logging
- No proper error type or monad transformer stack (Either/ExceptT)

### 2. HTML Injection Vulnerability (XSS Risk)
- **CRITICAL SECURITY ISSUE**: No HTML escaping in `renderEntry` (`src/Blogroll/Html.hs:174-188`)
- Feed titles and URLs are directly embedded in HTML without sanitization
- Malicious feed content could execute arbitrary JavaScript

### 3. Partial Functions
- Unsafe use of `listToMaybe` patterns without proper Nothing handling
- String-to-time parsing without validation can fail silently
- No handling for malformed XML structure

## 🟡 Major Improvements Needed

### 4. Code Duplication
- `fetchBlogrollOpml` and `readBlogrollOpml` (`src/Blogroll/Opml.hs`) share identical parsing logic
- Common parsing logic should be extracted into a separate function

### 5. Resource Management
- No connection pooling or timeouts for HTTP requests
- Unbounded concurrent requests via `mapConcurrently` could overwhelm system resources
- No retry logic for failed requests
- Large favicon fetching (128px) embedded directly in CSS is inefficient

### 6. Type Safety Issues
- Stringly-typed feed type handling (using "rss" strings instead of proper ADT)
- Fragile text manipulation for domain extraction (`src/Blogroll/Fetch.hs:60-62`)
- Magic constants throughout (25 entries, 128px favicons, etc.) should be configuration

### 7. Missing Input Validation
- No URL validation before making HTTP requests
- No XML schema validation
- No bounds checking on parsed data

## 🟢 Good Practices Observed

- Proper use of `async` library for concurrent operations
- Clear qualified imports improving readability
- Good separation of concerns across modules
- Modern GHC2024 language pragmas
- Consistent use of Text instead of String

## Recommended Refactorings

### 1. Introduce Proper Error Handling

```haskell
-- Add a proper error type
data BlogrollError 
  = ParseError Text
  | FetchError Text 
  | FileError Text
  | ValidationError Text
  deriving (Show, Eq)

-- Use ExceptT monad transformer
type BlogrollM = ExceptT BlogrollError IO

-- Replace error calls with:
fetchBlogrollOpml :: Text -> BlogrollM OpmlFeed
fetchBlogrollOpml urlOrPath = do
  when (not $ isValidUrl urlOrPath) $
    throwError $ ValidationError "Invalid URL format"
  -- ... rest of implementation
```

### 2. Fix HTML Escaping (CRITICAL)

```haskell
-- Use a proper HTML combinator library
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)

renderEntry :: FeedEntry -> Html
renderEntry entry = H.li $ do
  H.div $ do
    H.a ! A.href (toValue $ entryLink entry) 
        ! A.class_ (toValue $ generateDomainCssClass domain) $ 
        toHtml (entryTitle entry)  -- Properly escaped
    H.span ! A.class_ "source" $ 
      toHtml $ "(" <> extractDomain (entrySiteUrl entry) <> ")"
  H.div ! A.class_ "date" $ 
    toHtml $ formatTime defaultTimeLocale "%Y-%m-%d" (entryDate entry)
```

### 3. Extract Configuration

```haskell
data Config = Config
  { recentPostLimit :: Int        -- Default: 25
  , faviconSize :: Int            -- Default: 128
  , httpTimeout :: Int            -- Default: 30 seconds
  , maxConcurrentRequests :: Int  -- Default: 10
  , retryAttempts :: Int          -- Default: 3
  , fontPath :: FilePath
  , outputDir :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config 
  { recentPostLimit = 25
  , faviconSize = 128
  , httpTimeout = 30000
  , maxConcurrentRequests = 10
  , retryAttempts = 3
  , fontPath = "IBMPlexSans-Regular.woff2"
  , outputDir = "."
  }
```

### 4. Add Proper URL Validation

```haskell
import Network.URI (parseURI, uriAuthority, uriRegName)

data ValidatedUrl = ValidatedUrl 
  { originalUrl :: Text
  , parsedUri :: URI
  , domain :: Text
  }

validateUrl :: Text -> Either Text ValidatedUrl
validateUrl url = do
  uri <- maybe (Left "Invalid URL format") Right $ 
         parseURI (T.unpack url)
  auth <- maybe (Left "No domain in URL") Right $ 
          uriAuthority uri
  let domain = T.pack $ uriRegName auth
  return $ ValidatedUrl url uri domain

extractDomain :: Text -> Either Text Text
extractDomain url = domain <$> validateUrl url
```

### 5. Implement Rate Limiting

```haskell
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Network.HTTP.Client

data FetchConfig = FetchConfig
  { maxConcurrent :: Int
  , timeout :: Int
  , retryPolicy :: RetryPolicy
  }

fetchAllWithLimit :: FetchConfig -> [Text] -> IO [Either FetchError FeedData]
fetchAllWithLimit config urls = do
  sem <- newQSem (maxConcurrent config)
  manager <- newManager defaultManagerSettings 
    { managerResponseTimeout = responseTimeoutMicro (timeout config * 1000000) }
  
  mapConcurrently (rateLimitedFetch sem manager) urls
  where
    rateLimitedFetch sem mgr url = 
      bracket_ (waitQSem sem) (signalQSem sem) $
        retryWithPolicy (retryPolicy config) $ 
          fetchFeedWithManager mgr url
```

### 6. Improve OPML Parsing

```haskell
-- Extract common parsing logic
parseOpmlDocument :: L8.ByteString -> Either BlogrollError OpmlFeed
parseOpmlDocument xmlContent = do
  doc <- first (ParseError . T.pack . show) $ parseLBS def xmlContent
  let cursor = fromDocument doc
  validateOpmlStructure cursor
  return $ extractOpmlFeed cursor

-- Separate validation from extraction
validateOpmlStructure :: Cursor -> Either BlogrollError ()
validateOpmlStructure cursor = do
  when (null $ cursor $// element "opml") $
    Left $ ParseError "Not a valid OPML document"
  when (null $ cursor $// element "body") $
    Left $ ParseError "OPML missing body element"
  return ()
```

## Performance Considerations

1. **Favicon Optimization**:
   - Consider lazy loading favicons
   - Implement caching mechanism
   - Use smaller favicon size (32px instead of 128px)

2. **Feed Fetching**:
   - Add ETag/Last-Modified support for caching
   - Implement incremental updates
   - Consider pagination for large feed lists

3. **Memory Usage**:
   - Stream large files instead of loading entirely into memory
   - Use lazy ByteStrings where appropriate
   - Consider using conduit/pipes for streaming XML parsing

## Testing Recommendations

### Unit Tests
```haskell
-- test/Spec.hs
import Test.Hspec
import Blogroll.Feed
import Blogroll.Opml

spec :: Spec
spec = do
  describe "Feed Parsing" $ do
    it "parses valid RSS feed" $ do
      result <- parseFeed validRssSample
      length result `shouldBe` 10
    
    it "handles malformed RSS gracefully" $ do
      result <- parseFeed malformedRss
      result `shouldBe` Left (ParseError "...")

  describe "URL Validation" $ do
    it "accepts valid URLs" $ do
      validateUrl "https://example.com/feed.xml" 
        `shouldSatisfy` isRight
    
    it "rejects invalid URLs" $ do
      validateUrl "not a url" 
        `shouldSatisfy` isLeft
```

### Property Tests
```haskell
import Test.QuickCheck

prop_escaping_prevents_injection :: Text -> Bool
prop_escaping_prevents_injection input =
  let escaped = escapeHtml input
  in not (T.isInfixOf "<script>" escaped)
```

## Documentation Needs

1. Add Haddock documentation to all exported functions
2. Create comprehensive README with:
   - Installation instructions
   - Usage examples
   - Configuration options
   - API documentation
3. Add inline comments for complex algorithms
4. Document expected OPML format and limitations

## Security Checklist

- [ ] Implement HTML escaping for all user content
- [ ] Add Content Security Policy headers to generated HTML
- [ ] Validate and sanitize all URLs before fetching
- [ ] Implement request timeouts
- [ ] Add rate limiting for external requests
- [ ] Validate XML against schema
- [ ] Implement proper error messages (don't leak internal details)
- [ ] Add logging for security events

## Priority Action Items

1. **IMMEDIATE**: Fix HTML escaping vulnerability to prevent XSS
2. **HIGH**: Replace all `error` calls with proper error handling
3. **HIGH**: Add input validation for URLs and XML parsing
4. **HIGH**: Implement connection timeouts and rate limiting
5. **MEDIUM**: Extract configuration values
6. **MEDIUM**: Add comprehensive test suite
7. **MEDIUM**: Reduce code duplication
8. **LOW**: Add caching mechanisms
9. **LOW**: Optimize favicon handling
10. **LOW**: Add proper documentation

## Conclusion

The codebase demonstrates good fundamental Haskell practices with proper use of types and functional patterns. However, it requires immediate attention to security vulnerabilities (particularly HTML injection) and error handling before it can be considered production-ready. The suggested refactorings would significantly improve the robustness, maintainability, and security of the application.