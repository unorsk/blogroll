# AGENTS.md - Blogroll Project Guide

## Build/Lint/Test Commands
- **Build**: `cabal build` (or `cabal build --enable-optimization=2` for releases)
- **Run**: `cabal run blogroll <blogroll-file>` (generates HTML from feed URLs)
- **Nix**: `nix build` for the binary, `nix develop` for a dev shell (local use only, not CI)
- **Update deps**: `cabal update`
- **VCS**: use `jj` (jujutsu) for all version control; git is only used to push release tags (`scripts/release.sh`)
- **No tests configured** - add to cabal file if implementing tests

## Architecture & Structure
- **Language**: Haskell (GHC2024 standard, GHC 9.12.2+)
- **Build tool**: Cabal (blogroll.cabal is source of truth for version)
- **Modules**: app/Main.hs (executable), src/Blogroll/* (library: Feed, Fetch, Html, Type)
- **Data flow**: URLs → Fetch → Parse → Sort → HTML render
- **No database/external APIs** - pure file-based RSS aggregation

## Code Style Guidelines
- **Extensions**: OverloadedStrings, OverloadedRecordDot, DuplicateRecordFields, MultilineStrings
- **Imports**: Qualified imports for standard libraries (Data.Text qualified as T)
- **Naming**: camelCase functions, PascalCase types, record fields prefixed with type name (entryTitle)
- **Error handling**: Non-fatal problems are collected as `Warning` values (Blogroll.Type) and printed to stderr; avoid `error`
- **Formatting**: -Wall warnings enabled, explicit type signatures, clean record syntax
- **Security**: TODO items note string concatenation risks - prefer xml-conduit/lucid
