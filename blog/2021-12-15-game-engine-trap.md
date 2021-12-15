# The game engine trap

*This is part 3 of a trilogy.
You might like to read [part 1].*

[part 1]: /blog/new-website-part-1/

There's a bit of a meme going 'round in the gamedev community.

A beginner dev has a great idea for a game.
But instead of using a preëxisting game engine, they decide to build one themselves.
Months pass.
Thousands of lines of code are written.
They still don't have a game.

This is the *[game engine trap]*.

[game engine trap]: https://www.reddit.com/r/gamedev/wiki/engine_faq/#wiki_so...why_do_i_need_one_of_these.3F

When I started building my new website, I thought I'd create the perfect static site generator for it.
It would boast incremental and parallel builds, have a built-in preview server with hot reload, and above all---be 100% written in Rust.

It took a couple weekends before I realized my mistake.

The [final setup] is a lot less ambitious:

[final setup]: https://github.com/lambda-fairy/lambda-fairy.github.io

- The build is orchestrated via a [Makefile].
  Since Make is already incremental and parallel, I don't have to implement those features myself.
- The preview server is `python3 -m http.server`.
- I rebuild on changes with [entr].
- There is no hot reload.
  When I make a change, I tab to the browser and press F5.

[Makefile]: https://github.com/lambda-fairy/lambda-fairy.github.io/blob/main/Makefile
[entr]: https://eradman.com/entrproject/

The only bespoke component is a small Rust binary that processes each individual page.
Even then, most of that code is copied from [another project], so it isn't that bespoke after all.

[another project]: https://github.com/lambda-fairy/maud/blob/main/docs/

## The moral of the story

If there's any conclusion that could be had from this, it's: **know what you want!**

I realized early on that I didn't care about how the website was built.
By minimizing that part of the project, I could focus on what I really wanted to do: [fuck around with CSS 😎]

[fuck around with CSS 😎]: /blog/new-website-part-2/
