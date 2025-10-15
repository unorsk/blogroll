# So I don't forget about it. Sort of.

- [ ] Make adding new items to the blogroll easy (I just wanted to add this one: https://anchor.fm/s/10395bc40/podcast/rss)
- [ ] The renderHtml and generateOpmlXml functions - use xml-conduit or lucid instead of concatenating strings (security)
- [ ] The extractDomain function assumes all URLs start with https://. It will not work correctly for http:// URLs
- [ ] Would be nice if it could understand stuff like `<itunes:explicit>`