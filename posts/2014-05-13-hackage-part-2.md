---
title: Hackage update, part 2
tags: haskell
---

[Hi everybody!][Hi Dr Nick!]

There's still a few weeks before the Summer begins, so I'm taking the time to prepare. Among other things, this involves setting up a development server, to test the code I write.

Building and running the Hackage server went mostly without a hitch. Here are the issues I encountered:

* The first time I ran the server, it displayed this error:

        hackage-server: It looks like you are running the server without installing
        it. That is fine but you will have to give the location of the static html
        files with the --static-dir flag.

    Passing in `--static-dir=datafiles` seemed to stop the whining.

* By default, the `admin` account could not upload packages. Fortunately, this issue was easy to fix: simply add the account to the "uploaders" group.

* When I uploaded a package, the documentation did not appear. This is because building packages is handled by a separate program, `hackage-build`. Running it solved the problem:

        dist/build/hackage-build/hackage-build init http://localhost:8080
        dist/build/hackage-build/hackage-build build

    Alternatively, one could upload documentation manually, which is what I tried first.

[Here's what it looks like.][Screenshot] Beautiful, isn't it?

[Hi Dr Nick!]: https://www.youtube.com/watch?v=YlmECL2ED2I
[Screenshot]: https://imgur.com/iYz3962
