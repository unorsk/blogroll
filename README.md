# RSS Blogroll

A simple RSS reader (written in Haskell btw)

## Demo (my personal blogroll)

The RSS reader is automatically deployed to: **https://unorsk.github.io/blogroll/**

## Usage

```
blogroll BLOGROLL_FILE [-t|--title TITLE] [-f|--font FONT_PATH] [-n|--recent N]
```

`BLOGROLL_FILE` is a plain text file with one feed URL per line (see `blogroll.txt`).
The output is `index.html` (the `N` most recent entries, 25 by default) and
`all.html` (everything), written to the current directory.

By default the pages use the system sans-serif font. Pass `-f path/to/font.woff2`
to embed a custom font instead.

### Building and running

With cabal:

```
cabal run blogroll -- blogroll.txt -t "Test Blogroll"
```

With nix:

```
nix build            # binary in ./result/bin/blogroll
nix develop          # shell with GHC and cabal
```

## Releasing

Bump `version` in `blogroll.cabal`, add a `CHANGELOG.md` entry, then run
`scripts/release.sh` to tag and push. The tag triggers the release build;
the daily GitHub Actions job picks up the latest release binary, runs it
against `blogroll.txt` and publishes the result to GitHub Pages.

### My todo list. Sort of.

- [ ] Make adding new items to the blogroll easy (I just wanted to add this one: https://anchor.fm/s/10395bc40/podcast/rss)
- [ ] Would be nice if it could understand stuff like `<itunes:explicit>`
- [ ] Make github jobs be more fork-friendly (configurable)
- [x] Setup nix
- [x] Make it possible to use custom fonts
- [ ] ...and styles

## License

MIT License
