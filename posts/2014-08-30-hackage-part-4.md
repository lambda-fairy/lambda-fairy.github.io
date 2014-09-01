---
title: Hackage update, part 4
tags: haskell
---

A lot has happened with Hackage since my [last update][part 3]. Now that the Summer of Code is over, I'll summarize the work I've done since then, and outline where this project will go next.

[part 3]: /blog/hackage-part-3/


What I've done since then
=========================

In my [latest patch][#2025], I extended build reporting to cover both *planning failures* and *package candidates*.

[#2025]: https://github.com/haskell/cabal/pull/2025


Reporting planning failures
---------------------------

As I mentioned in my [last post][part 3], the Hackage build bot submits *build reports* for every package it attempts to build. If the documentation for a package is missing, these reports can help us understand what caused the problem.

A *planning failure* is when cabal-install cannot find a consistent set of dependencies to use. You can trigger a planning failure yourself:

    $ cabal install robot --constraint='robot < 1.1' --constraint='robot > 1.1'
    cabal: Could not resolve dependencies:
    ...

Since we can't have a `robot` which is both older *and* newer than 1.1, the resolver fails.

Formerly, as dependency resolution ran early in the build process, any failures at this stage were not reported. So if the build bot encountered a planning failure, all the user saw was missing documentation, with no hints as to what went wrong.

The fix was mostly straightforward, save for one issue: since users can report their own builds, a naïve implementation would have lead to Hackage being swamped with frivolous reports. So this feature is enabled only in the build bot, and disabled otherwise.


Reporting candidate builds
--------------------------

Hackage has a feature called *build candidates*. This lets package maintainers upload and test packages without publishing them to the main site.

There are a few gaps in the implementation, however. One of them was the lack of reporting: when a candidate was uploaded, the build bot would compile the package but not submit a report. This was a major issue, since this reporting was what motivated the feature in the first place.

After some digging, I traced this to [a bug in cabal-install][#1189]. A candidate was not published in the main package index, so it was impossible to refer to one by name (e.g. `hello-1.0`). So the bot used URLs (e.g. `http://hackage.haskell.org/.../hello-1.0.tar.gz`) instead.

[#1189]: https://github.com/haskell/cabal/issues/1189

The problem was if only a URL was given, cabal-install considered it a "local" package and refused to generate a report. My fix was to rewrite the reporting logic so that it no longer made this distinction.


Where to next?
==============

Though the Summer of Code has ended, my work with Hackage has not. There are still many issues that need clearing up, especially with the [candidates feature][candidates bugs]; I'll continue hacking away at them in my spare time.

[candidates bugs]: https://github.com/haskell/hackage-server/labels/component:%20candidates

And lest I forget --- many thanks to my mentor Duncan Coutts for his guidance throughout this project! Time zone issues aside, I had plenty of fun this summer, and learned just as much.
