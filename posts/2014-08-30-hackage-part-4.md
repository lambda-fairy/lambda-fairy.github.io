---
title: Hackage update, part 4
tags: haskell
---

A lot has happened with Hackage since my [last update][part 3]. Now that the Summer of Code is over, I'll summarize the work I've done since then, and outline where this project will go next.

[part 3]: /blog/hackage-part-3/


What's "build reporting"?
=========================

Since my project covered a few obscure parts of Hackage and Cabal, I think it's worthwhile to clear some terminology first.

If you've uploaded a library to [Hackage][] before, you may have noticed that the Haddock documentation does not appear straight away. Since building a package can be quite resource intensive, the job is handled by a dedicated *build bot*. This bot continually polls for new packages, invokes `cabal install` on them (with some special flags, which I'll go into later), and uploads the result.

[Hackage]: https://hackage.haskell.org/

Of course, this process does not always succeed. If a package fails to compile, then it will not have any documentation either. This is clearly very inconvenient.

Fortunately, recent versions of `cabal` include a feature called *build reporting*. When invoked with the `--build-summary` option, `cabal` creates a file containing useful information about the build. Here's an example using the `robot` package:

    $ cabal install robot --build-summary='$pkg.report'
    ...
    $ cat robot.report
    package: robot-1.3.0.1
    os: linux
    arch: x86_64
    compiler: ghc-7.6.3
    client: cabal-install-1.20.0.3
    dependencies: xhb-0.5.2014.4.10 transformers-0.3.0.0
                  exceptions-0.6.1 containers-0.5.0.0 base-4.6.0.1
    install-outcome: InstallOk
    docs-outcome: NotTried
    tests-outcome: NotTried

Since the build bot uses `cabal`, it has access to these reports as well. So whenever the bot completes a build --- successful or not --- it posts the corresponding report to Hackage. You can read these reports yourself via a special URL; for our `robot` example it's <http://hackage.haskell.org/package/robot-1.3.0.1/reports/>.

In summary: if the docs for a package are missing, then the reports will tell us why. If there are no reports, then it must mean the build bot hasn't attempted the package yet. All is fine and dandy, at least in theory.


Unfortunately...
================

... not all builds were reported. The gaps were in two places: *planning failures* and *package candidates*. My [latest patch][#2025] to `cabal` fixed both these issues.

[#2025]: https://github.com/haskell/cabal/pull/2025


Reporting planning failures
---------------------------

A *planning failure* is when cabal-install cannot find a consistent set of dependencies to use. You can trigger a planning failure yourself:

    $ cabal install robot --constraint='robot < 1.1' --constraint='robot > 1.1'
    cabal: Could not resolve dependencies:
    ...

Since we can't have a `robot` which is both older *and* newer than 1.1, the resolver fails.

Formerly, as dependency resolution ran early in the build process, any failures at this stage did not generate a corresponding report. So if the build bot encountered a planning failure, all the user saw was missing documentation, with no hints as to what went wrong.

The fix was mostly straightforward, save for one issue: since users can report their own builds, a naïve implementation would have lead to Hackage being swamped with frivolous reports. So this feature is guarded behind a flag (`--report-planning-failure`), and disabled by default.


Reporting candidate builds
--------------------------

Hackage has a feature called *build candidates*. This lets package maintainers upload and test packages without publishing them to the main site.

Again, the problem was the lack of reporting: when a candidate was uploaded, the build bot would compile the package but not submit a report. This was a major issue, since this reporting was what motivated the feature in the first place.

After some digging, I traced this to [a bug in `cabal`][#1189]. A candidate is not published in the main package index (by definition), so it is impossible to refer to one by name (e.g. `hello-1.0`). So the build bot invokes `cabal` using the bare URL instead (e.g. `http://hackage.haskell.org/package/hello/candidates/hello-1.0.tar.gz`).

[#1189]: https://github.com/haskell/cabal/issues/1189

The problem was if only a URL was given, `cabal` considered it a "local" package and did not generate a report. The reason for this behavior is outside the scope of this post, but the fix was clear: change `cabal` to generate reports for all packages, no matter how they are specified on the command line.


Where to next?
==============

Though the Summer of Code has ended, my work with Hackage has not. There are still many issues that need clearing up, especially with the [candidates feature][candidates bugs]; I'll continue hacking away at them in my spare time.

[candidates bugs]: https://github.com/haskell/hackage-server/labels/component:%20candidates

And lest I forget --- many thanks to my mentor Duncan Coutts for his guidance throughout this project! I had plenty of fun this summer, and learned just as much.
