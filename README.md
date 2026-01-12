# RSS Blogroll

A simple RSS reader (written in Haskell btw)

## Demo (my personal blogroll)

The RSS reader is automatically deployed to: **https://unorsk.github.io/blogroll/**


### Generating the blogroll
```
cabal run blogroll -- blogroll.txt "Test Blogroll" "IBMPlexSans-Regular.woff2"
```

### My todo list. Sort of.

- [ ] Make adding new items to the blogroll easy (I just wanted to add this one: https://anchor.fm/s/10395bc40/podcast/rss)
- [ ] Would be nice if it could understand stuff like `<itunes:explicit>`
- [ ] Make github jobs be more fork-friendly (configurable)
- [ ] Setup nix
- [x] Make it possible to use custom fonts
- [ ] ...and styles

## License

MIT License