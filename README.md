# RSS Feed Reader

A simple Haskell-based RSS reader that aggregates feeds and generates a beautiful HTML page with automatic GitHub Pages deployment.

## Features

- Fetches RSS/Atom feeds from a configurable list
- Parses and merges entries from multiple feeds
- Sorts entries by publication date (newest first)
- Generates a clean HTML page with IBM Plex Sans typography
- Automated daily updates via GitHub Actions
- GitHub Pages deployment

## Live Site

The RSS reader is automatically deployed to: **https://unorsk.github.io/feed/**

## How It Works

### 1. Feed Configuration

Edit `blogroll.txt` to add or remove RSS/Atom feed URLs (one per line):

```
https://matklad.github.io/feed.xml
https://andrewkelley.me/rss.xml
https://mitchellh.com/feed.xml
https://www.dbreunig.com/feed.xml
https://mmapped.blog/feed.xml
https://notes.eatonphil.com/rss.xml
```

### 2. Automated Updates

The system uses two GitHub Actions workflows:

#### Build and Release Workflow (`build-release.yml`)
- **Triggers**: When you push a git tag (e.g., `v1.0.1`)
- **Purpose**: Builds an optimized Haskell binary and creates a GitHub release
- **Use case**: When you modify the Haskell code

#### Daily Feed Update Workflow (`daily-feed.yml`)
- **Triggers**: Daily at midnight UTC (automatic)
- **Purpose**: Downloads the latest release, runs the RSS reader, and deploys to GitHub Pages
- **Use case**: Regular feed updates

## Making Changes

### Updating Feed Sources

To add or remove RSS feeds:

1. Edit `blogroll.txt` directly on GitHub or locally
2. Commit and push the changes
3. The next daily run (or manual trigger) will use the updated feed list

**No new release needed** - the daily job always uses the latest `blogroll.txt` from the main branch.

### Updating Code

When you modify the Haskell code:

1. Make your changes to `app/Main.hs` or `blogroll.cabal`
2. Commit and push the changes
3. Create a new release tag:
   ```bash
   git tag v1.0.1
   git push origin v1.0.1
   ```
4. This automatically builds and releases the new version
5. Future daily runs will use the new release

## Manual Operations

### Manually Trigger Daily Update

To manually run the feed update (useful for testing or immediate updates):

```bash
gh workflow run daily-feed.yml
```

You can also trigger it from the GitHub web interface:
1. Go to Actions tab
2. Select "Daily Feed Update" workflow  
3. Click "Run workflow"

### Check Workflow Status

```bash
# List recent workflow runs
gh run list --limit 5

# View details of a specific run
gh run view <run-id>

# View logs of a failed run
gh run view <run-id> --log-failed
```

### View Current Release

```bash
# List all releases
gh release list

# View latest release details
gh release view --web
```

## Local Development

### Prerequisites

- GHC 9.12.2 or later
- Cabal
- The project uses `GHC2024` language standard

### Building Locally

```bash
# Update dependencies
cabal update

# Build the project
cabal build

# Run the RSS reader
cabal run

# This generates index.html in the current directory
```

### Project Structure

```
├── app/Main.hs              # Main Haskell application
├── blogroll.cabal           # Cabal project configuration
├── blogroll.txt             # RSS feed URLs (one per line)
├── IBMPlexSans-VariableFont_wdth,wght.ttf  # Font file
├── .github/workflows/
│   ├── build-release.yml    # Release build workflow
│   └── daily-feed.yml       # Daily update workflow
└── README.md                # This file
```

## Architecture

1. **Feed Fetching**: Downloads RSS/Atom feeds via HTTP
2. **Parsing**: Uses `xml-conduit` to parse XML and extract entries
3. **Date Handling**: Supports both RSS `pubDate` and Atom `published`/`updated` fields
4. **Sorting**: Merges all entries and sorts by publication date (newest first)
5. **HTML Generation**: Creates responsive HTML with embedded CSS and typography
6. **Deployment**: GitHub Actions automatically deploys to GitHub Pages

## Dependencies

- `http-conduit`: HTTP client for fetching feeds
- `xml-conduit`: XML parsing and processing
- `time`: Date/time parsing and formatting
- `text`: Text processing
- `bytestring`: Binary data handling
- `containers`: Data structures

## Troubleshooting

### Daily Job Fails

Check the workflow logs:
```bash
gh run list --limit 3
gh run view <failed-run-id> --log-failed
```

Common issues:
- Network timeout fetching feeds
- Invalid XML in one of the feeds
- GitHub Pages deployment permissions

### Release Build Fails

Ensure you have the correct permissions and the Haskell code compiles locally:
```bash
cabal build --enable-optimization=2
```

### Adding New Dependencies

1. Add to `build-depends` in `blogroll.cabal`
2. Import in `app/Main.hs`
3. Create a new release tag to deploy the updated binary

## License

MIT License - feel free to fork and modify for your own RSS reading needs!