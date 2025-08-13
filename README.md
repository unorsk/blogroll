# RSS Blogroll

A simple Haskell-based RSS reader that aggregates feeds and generates a beautiful HTML page with automatic GitHub Pages deployment.

## Features

- Fetches RSS/Atom feeds from a configurable list
- Parses and merges entries from multiple feeds
- Sorts entries by publication date (newest first)
- Generates a clean HTML page with IBM Plex Sans typography
- Automated daily updates via GitHub Actions
- GitHub Pages deployment

## Live Site

The RSS reader is automatically deployed to: **https://unorsk.github.io/blogroll/**

## How It Works

### 1. Feed Configuration

Edit `blogroll.opml.xml` to add or remove RSS/Atom feeds.

Or generating it with blogroll

```
cat blogroll.txt | cabal run blogroll -- --generate-opml > blogroll.opml.xml
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
2. Update the version in `blogroll.cabal` (the cabal file is the source of truth)
3. Update `CHANGELOG.md` with release notes for the new version
4. Commit and push the changes
5. Create a new release using the automated script:
   ```bash
   ./scripts/release.sh
   ```
   This script automatically:
   - Reads the current version from the cabal file
   - Creates a git tag (e.g., `v1.0.1`) 
   - Pushes the tag to trigger the release workflow

6. GitHub Actions automatically builds and releases the new version
7. Future daily runs will use the new release

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

# This generates index.html/all.html in the current directory
```

### Project Structure

```
├── app/Main.hs              # Main Haskell application
├── blogroll.cabal           # Cabal project configuration (source of truth for version)
├── blogroll.opml.xml        # RSS feeds OPML file
├── CHANGELOG.md             # Release notes and version history
├── IBMPlexSans-VariableFont_wdth,wght.ttf  # Font file
├── scripts/
│   └── release.sh           # Automated release script
├── .github/workflows/
│   ├── build-release.yml    # Release build workflow
│   └── daily-feed.yml       # Daily update workflow
└── README.md                # This file
```

## Troubleshooting

### Daily Job Fails

Check the workflow logs:
```bash
gh run list --limit 3
gh run view <failed-run-id> --log-failed
```

## License

MIT License - feel free to fork and modify for your own RSS reading needs!