# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Blogroll is an RSS feed aggregator written in Haskell that generates static HTML pages from a list of RSS/Atom feed URLs. The application fetches feeds concurrently, parses entries, and renders them as styled HTML with embedded favicons. It's deployed daily via GitHub Actions to GitHub Pages.

## Build and Development Commands

```bash
# Build the project
cabal build

# Build with optimizations (for releases)
cabal build --enable-optimization=2

# Run the application
cabal run blogroll <path-to-blogroll.txt>

# Install binary locally
cabal install --install-method=copy --installdir=./bin

# Update Cabal package list
cabal update
```

## Architecture

### Core Data Flow

```
blogroll.txt (URLs)
  → Blogroll.Fetch (concurrent HTTP fetching + favicons)
  → Blogroll.Feed (RSS/Atom parsing)
  → Blogroll.Html (HTML generation with CSS)
  → index.html (25 recent) + all.html (all entries)
```

### Module Structure

- **app/Main.hs**: Entry point that reads URL file and orchestrates rendering
- **src/Blogroll/Type.hs**: Core data types (`Blogroll`, `FeedEntry`)
- **src/Blogroll/Fetch.hs**: Concurrent HTTP fetching for feeds and favicons (using async)
- **src/Blogroll/Feed.hs**: RSS/Atom XML parsing and time format handling
- **src/Blogroll/Html.hs**: HTML generation with embedded CSS and base64 favicons

### Concurrency Model

The application uses `async` for concurrent operations:
- Fetches all feeds in parallel via `mapConcurrently`
- Fetches favicons for all domains in parallel
- Timing information is logged for each fetch operation

### HTML Generation Strategy

- Generates two files: `index.html` (25 most recent entries) and `all.html` (all entries)
- Embeds favicons as base64-encoded data URIs in CSS pseudo-elements
- Uses domain-based CSS classes to associate favicons with entries
- Uses system font stack for typography (no custom fonts)

## Dependencies

Key external libraries:
- `http-conduit`: HTTP client for fetching feeds and favicons
- `xml-conduit`: XML parsing for RSS/Atom feeds
- `async`: Concurrent feed fetching
- `base64-bytestring`: Favicon encoding
- `time`: Date parsing and formatting (RFC 822, ISO 8601)

## Release Process

1. Update version in `blogroll.cabal`
2. Run `scripts/release.sh` which:
   - Extracts version from cabal file
   - Creates and pushes git tag `v<version>`
3. GitHub Actions workflow `build-release.yml` triggers on tag push:
   - Builds optimized binary
   - Creates release with `blogroll.tar.gz` artifact

## GitHub Actions Workflows

### build-release.yml (on tag push)
- Builds with GHC 9.12.2 and optimization level 2
- Caches Cabal store and build artifacts
- Uploads binary as GitHub release asset
- Artifact retained for 90 days for daily feed job

### daily-feed.yml (daily at midnight UTC, or manual trigger)
- Downloads latest release binary (or builds from source if no release exists)
- Fetches latest `blogroll.txt` from main branch
- Generates HTML files
- Deploys to GitHub Pages

## Code Style

- **Language Standard**: GHC2024
- **GHC Version**: 9.12.2+
- **Warnings**: `-Wall` enabled
- **Common Extensions**:
  - `OverloadedStrings`, `OverloadedRecordDot`
  - `DuplicateRecordFields`, `MultilineStrings`
- **Import Style**: Qualified imports for standard libraries (`Data.Text qualified as T`)
- **Record Fields**: Prefixed with type name (e.g., `entryTitle`, `entryLink`)

## Known Issues and TODOs

1. **Error Handling**: Currently using bare `error` calls; should migrate to `Either`/`ExceptT` for better error handling
2. **Security**: String concatenation in HTML generation creates XSS risks; should migrate to `lucid` or similar HTML DSL
3. **Testing**: No test suite currently configured in cabal file

## Input File Format

`blogroll.txt` contains one RSS/Atom feed URL per line:
```
https://example.com/feed.xml
https://another.example/rss
```

Empty lines are ignored. The file is read from the path provided as the first CLI argument.

## HTML Output

- **index.html**: Shows 25 most recent entries across all feeds, sorted by date (newest first)
- **all.html**: Shows all entries, sorted by date
- Both files include:
  - Inline CSS with embedded favicons
  - System font stack for typography
  - Responsive design (max-width: 800px)
  - Date formatting: YYYY-MM-DD

## Deployment

The application is deployed to GitHub Pages at: https://unorsk.github.io/blogroll/

Daily updates are automated via GitHub Actions, which fetches fresh feed data and regenerates the HTML pages.
