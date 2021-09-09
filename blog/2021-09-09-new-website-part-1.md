# New website: Why?

I launched my new website this week.
It's simpler, nicer to work on, and---in the words of [smlt]---"hot".

[smlt]: https://twitter.com/crombird

But my [original website] had served me for a long time; and to be honest, it still holds up today.
Why rewrite it?

[original website]: https://web.archive.org/web/20210304054348/https://lambda.xyz/

## Technical reasons

When I built my website in 2013, I used the best tools that I knew of at the time.

- The [Sass CSS preprocessor][Sass] gave me variables and mixins, which let me enforce a consistent color scheme and organize my clearfix hacks.

[Sass]: https://sass-lang.com/

- The [Hakyll static site generator][Hakyll] gave me unfettered control over every aspect of the site.
  It was also written in Haskell, which (to my teenage self) was really cool.

[Hakyll]: https://jaspervdj.be/hakyll/

But these value propositions don't really hold today.

- Modern CSS is powerful.
  I can use CSS variables to share colors and fonts.
  Flexbox and Grid let me express the layout I want without resorting to hacks.
  Sass might still be useful for larger sites, but my personal website can thrive on CSS alone.

- Hakyll isn't as practical as it used to be.
  With most of my work being in Dart or Rust these days, I'm finding it harder to justify maintaining a Haskell environment just for this one project.

  Moreover, Haskell builds are *slow*---an order of magnitude slower than C++ or Rust.
  My GitHub Action took [*forty minutes*][that's four tens] to run.
  I expect those build times for an enterprise monolith, not a blog.[^cache]

  Also, I'm old.
  I don't care about being cool.

[that's four tens]: https://github.com/lambda-fairy/lambda-fairy.github.io/actions/runs/1153509822
[^cache]: Before you suggest a cache: I update this blog so infrequently that by the time I get to use a cached build, it would be out of date.

## Design reasons

But what really got me was the visual design.

First, the **color scheme**.
I love purple as much as I did back then, but I'm more picky now.
The reddish hue felt too bloody, too *puce*.
I hate puce.[^puce]

[^puce]: Who names a color after flea vomit? Seriously.

And the **minimalism**.
When skeuomorphism was all the rage, that style was a breath of fresh air.
But today, everyone is the same, Lighthouse-optimized, flat color drop shadow neo-grotesque rounded box.
Apple went from Garamond to Myriad to Helvetica.
Monochrome just isn't cool anymore.

But most importantly, that design **doesn't represent the person I am now**.
The 2021 me is quiet confidence.
Daisy chains and amethyst rings.
They're never in the spotlight, but everyone knows who they are.
The scent of lavender fills the room.

In my next post, I'll talk about the features of this new site.
