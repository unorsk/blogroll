# RSS Blogroll

A Haskell-based RSS/Atom feed aggregator that generates static HTML pages from a list of feed URLs. The application fetches feeds concurrently, parses entries, and renders them as styled HTML with embedded favicons.

## Features

- **Concurrent feed fetching** using Haskell's `async` library
- **RSS and Atom support** with robust date parsing (RFC 822, ISO 8601)
- **Favicon integration** - automatically fetches and embeds favicons as base64 data URIs
- **Static HTML generation** - no JavaScript required, fast and lightweight
- **Dual output** - generates both a recent view (25 entries) and a complete archive
- **System font stack** - no external font dependencies
- **Responsive design** - works on mobile and desktop

## Demo

See it in action: **https://unisay.github.io/blogroll/**

Daily automated updates via GitHub Actions ensure fresh content.

## Quick Start

### Using Nix (Recommended)

If you have Nix with flakes enabled:

```bash
# Build the project (dynamic linking)
nix build

# Build static binary (Linux only, musl + UPX compressed)
nix build .#blogroll-static

# Run directly
nix run . -- blogroll.txt

# Enter development shell with all dependencies
nix develop

# Inside the dev shell, you have access to:
cabal build
cabal run blogroll -- blogroll.txt
```

The Nix flake provides:
- **Dynamic builds** (default) - fast iteration with optimizations (`-O2`, `-split-sections`)
- **Static builds** (Linux only) - fully static musl binaries, stripped and UPX compressed (~4MB)
- **GHC 9.12.2** compiler environment
- **Development tools** - cabal, haskell-language-server
- **Cross-platform support** - x86_64-linux, aarch64-linux, x86_64-darwin, aarch64-darwin

**Note**: Static builds on Linux use musl cross-compilation and take several hours on first build (all dependencies must be rebuilt for musl). Subsequent builds are cached and fast. Binaries are automatically stripped and compressed with UPX 4.2.4 for minimal size.

### Using Cabal

```bash
# Update package list
cabal update

# Build the project
cabal build

# Run the application
cabal run blogroll -- blogroll.txt

# Install locally
cabal install --install-method=copy --installdir=./bin
./bin/blogroll blogroll.txt
```

## Input Format

Create a `blogroll.txt` file with one RSS/Atom feed URL per line:

```text
https://blog.example.com/feed.xml
https://another-blog.com/rss
https://yet-another.com/atom.xml
```

Empty lines are ignored.

## Output

The application generates two HTML files:

- **index.html** - Shows the 25 most recent entries across all feeds
- **all.html** - Shows all entries, sorted by date (newest first)

Both files include:
- Inline CSS with embedded favicons
- Responsive design (max-width: 800px)
- Date formatting: YYYY-MM-DD
- Domain-based favicon display

## Architecture

```
blogroll.txt (URLs)
  → Blogroll.Fetch (concurrent HTTP + favicons)
  → Blogroll.Feed (RSS/Atom parsing)
  → Blogroll.Html (HTML generation)
  → index.html + all.html
```

### Module Overview

- **app/Main.hs** - Entry point, orchestrates the rendering pipeline
- **src/Blogroll/Type.hs** - Core data types (`Blogroll`, `FeedEntry`)
- **src/Blogroll/Fetch.hs** - Concurrent HTTP fetching for feeds and favicons
- **src/Blogroll/Feed.hs** - RSS/Atom XML parsing with date format handling
- **src/Blogroll/Html.hs** - HTML generation with embedded CSS and base64 favicons

## Development

### Building with Optimizations

For production builds with maximum optimization:

```bash
cabal build --enable-optimization=2
```

For Nix users, builds are automatically optimized with `-O2` and `-split-sections`. Static builds additionally use musl, are stripped, and compressed with UPX 4.2.4 (achieving ~82% size reduction).

### Release Process

Create a new release:

```bash
# Update version in blogroll.cabal first, then:
./scripts/release.sh
```

This creates a git tag and triggers the GitHub Actions release workflow, which:
- Builds an optimized binary
- Creates a GitHub release
- Uploads `blogroll.tar.gz` artifact
- Makes the binary available for the daily feed job

### Project Structure

```
blogroll/
├── app/
│   └── Main.hs              # Application entry point
├── src/
│   └── Blogroll/
│       ├── Type.hs          # Data types
│       ├── Fetch.hs         # HTTP fetching
│       ├── Feed.hs          # RSS/Atom parsing
│       └── Html.hs          # HTML generation
├── .github/workflows/
│   ├── build-release.yml    # Release builds
│   └── daily-feed.yml       # Daily feed updates
├── blogroll.txt             # Feed URL list
├── blogroll.cabal           # Package metadata
├── cabal.project            # Cabal configuration
└── flake.nix                # Nix flake definition
```

## Dependencies

Key libraries:
- **http-conduit** - HTTP client for fetching feeds and favicons
- **xml-conduit** - XML parsing for RSS/Atom feeds
- **async** - Concurrent feed fetching
- **base64-bytestring** - Favicon encoding
- **time** - Date parsing and formatting

## GitHub Actions Workflows

### Build and Release (`build-release.yml`)

Triggers on tag push (e.g., `v1.0.8`):
- Builds optimized binary with GHC 9.12.2
- Creates GitHub release with `blogroll.tar.gz`
- Artifact retained for 90 days

### Daily Feed Update (`daily-feed.yml`)

Runs daily at midnight UTC (or manually):
- Downloads latest release binary (or builds from source if no release exists)
- Fetches fresh `blogroll.txt` from main branch
- Generates HTML files
- Deploys to GitHub Pages

## Deployment

The application is deployed to GitHub Pages with daily updates. The deployment process:

1. Fetches the latest feed URLs from the repository
2. Generates fresh HTML from all feeds
3. Deploys to `https://username.github.io/blogroll/`

## Technical Details

- **Language**: Haskell (GHC2024 standard)
- **GHC Version**: 9.12.2+
- **Build System**: Cabal 3.14+
- **Nix Flake**: Yes (with haskell.nix)

## License

MIT License
