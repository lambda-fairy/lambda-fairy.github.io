# Hackage update, part 3

Over the last few months, I've been working on [various improvements][proposal] to [Hackage][hackage] under the supervision of Duncan Coutts, as part of the [Google Summer of Code][gsoc]. As it's been a while since my last post, I thought I'd give a short summary of what I've done so far.

[proposal]: https://github.com/haskell/hackage-server/wiki/GSoC-2014
[hackage]: https://hackage.haskell.org/
[gsoc]: https://www.google-melange.com/gsoc/homepage/google/gsoc2014


## Documentation upload

Hackage has supported [manual documentation uploads][#56] for quite some time, albeit through a rather obscure API. This is an important feature for packages which cannot be built automatically on the server.

My contribution was a simple HTML interface, which allowed for uploading and deleting documentation at the click of a button. Here's how it looks (click to enlarge):

![](/images/2014/doc-upload.png)

There is still one pain point, however. Building the documentation archive in the first place is quite error-prone; it would be nice if Cabal could automate the process. As that is a Cabal issue, not a Hackage one, it is out of scope for now -- but would make a good project to tackle next.

[#56]: https://github.com/haskell/hackage-server/issues/56


## Build reports

The next feature I worked on was build reporting. The Hackage build bot leaves logs for every package it builds, and it is often helpful to view them. Users can submit their own reports as well, though this is rare in practice. Unfortunately, as with documentation uploads, while the functionality was there, it was [difficult to discover and use][fuuzetsu].

[fuuzetsu]: http://web.archive.org/web/20160929095712/http://fuuzetsu.co.uk/blog/posts/2014-01-06-Fix-your-Hackage-documentation.html

My contribution had two parts -- a fancy build status indicator (click to enlarge):

![Hooray!](/images/2014/br-status.png)

and a pretty build reports view:

![Build status... useful metadata... and a full log!](/images/2014/br-report.png)

This part of the project ended up a bit broader than expected, as there was plenty of information to summarize. The gory details are on the [wiki][bikeshed], for the curious.

[bikeshed]: https://github.com/haskell/hackage-server/wiki/Bikeshed:-build-reports


## Next steps

Overall, I'm happy with the work I've done so far, especially given that it's my first time working on a project of this scale.

Next up would most likely be either [package candidates][#41] or the tag system.

[#41]: https://github.com/haskell/hackage-server/issues/41
