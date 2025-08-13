# So I don't forget about it. Sort of.

- [ ] The renderHtml and generateOpmlXml functions - use xml-conduit or lucid instead of concatenating strings (security)
- [ ] The extractDomain function assumes all URLs start with https://. It will not work correctly for http:// URLs
- [ ] readBlogrollOpml and fetchBlogrollOpml use error, Change the return type to IO (Either String OpmlFeed) and handle the Either in main.
- [ ] parseOpmlEntries - only uses `type="rss"` fix this shit (both consuming and producing)
