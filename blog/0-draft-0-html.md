# Is HTML a programming language?

[Are fish wet?][fish]
Is [Stevonnie] a boy or a girl?

[fish]: https://www.youtube.com/watch?v=bubFg390wZY
[Stevonnie]: https://steven-universe.fandom.com/wiki/Stevonnie

Is HTML a programming language?

People love to argue about taxonomy.
It's right up there, along with the weather, the latest Netflix series, and whatever Josh has gotten up to this time.[^josh]
And like most arguments, they're driven more by knee-jerk reactions than truth.
Any reasonable person would recognize that such "debates" are a waste of time, and refuse to get sucked in.

[^josh]:
    Ah, Josh.

But I'm not a reasonable person.
And if you're still reading this article, you aren't either.

So let's dive head first into this mess, and explore this trashfire together!

## "It's not Turing complete!"

Indeed, it's impossible to simulate a Turing machine using HTML markup alone.
But does that make it not a programming language?

[Coq] is not Turing complete.
But it's powerful enough to build a [C compiler][CompCert].

[Coq]: https://en.wikipedia.org/wiki/Coq
[CompCert]: https://compcert.org/

On the other side, the TeX typesetting system isn't usually considered a programming language.
But [it *is* Turing complete][TeX TC].

[TeX TC]: https://pbelmans.ncag.info/blog/2010/12/12/a-turing-machine-in-latex-follow-u/

And if that's not enough---C, everyone's favorite programming language, is *not* Turing complete![^c]

[^c]:
    A Turing machine has infinite memory.
    But in C, since any memory location can be referenced by a pointer, this limits the available memory to 2<sup>POINTER_SIZE</sup> bytes.
    Hence standard C cannot be Turing complete.

From these examples, it's clear that Turing completeness and programming language-ness aren't the same thing.
But if Turing completeness doesn't make a programming language, then what does?

## "You can extract meaning from it without running arbitrary code!"

This is what the "Turing complete" people *actually* mean, and unlike that other claim, is actually relevant.

For most web pages, the bulk of their meaning is in their text, which can be extracted with little effort.
This is the idea behind [web scraping], and---to a first approximation---how search engines like Google work.

[web scraping]: https://en.wikipedia.org/wiki/Web_scraping

So there we have it.
HTML is not a programming language.

Right?

## "HTML isn't just a markup language, it's an application platform!"

- HTML standard recognizes the web as a platform for publishing documents *and* apps, and places equal weight on both.

## "Nobody writes *only* HTML!"

One of the most popular static site frameworks, [Gatsby], is built upon JavaScript and React.
Feel free to form your own opinion on this---but the reality is that contemporary web design is not restricted to HTML and CSS, but requires programming skills as well.

[Gatsby]: https://www.gatsbyjs.com/

But of course...

## "It doesn't matter."

And to an extent, it doesn't.
Whether HTML is a programming language or not has no relevance to the work that professional designers, developers, and browser vendors do.
They'll keep making cool shit regardless.

<!--
Where is *does* matter is how it looks to beginners.
For many people, their first exposure to code is through HTML.
I have fond memories of uploading my first site to some dodgy free web host, and showing it with pride to my middle-school friends.
I'm sure many of you have similar memories with Geocities, Myspace, or Tumblr.
The web has a low barrier to entry, and that is its greatest strength.

When we, as experts, see a statement like "HTML is not a programming language", we take it at face value---as a statement of fact.
But to a beginner, that sounds a lot like "HTML isn't programming".
-->

But here's the thing.
The term "programming language" is too vague to define in its own right.
Unlike more precise attributes, like "has syntax highlighting on GitHub" or "Turing complete" or "can run Doom", there is no single criterion that captures what everyone thinks a programming language should be.
So when someone says that HTML is or isn't a programming language, it's not a technical statement---it's a *political* one.
They are expressing their opinion on what programming *means*, and by implication, excluding anyone who doesn't fit.

This gets a bit suspicious when you realize that frontend development is [both lower paid *and* has more women][Guardian].
The idea that "HTML isn't programming" feeds into the disparity between frontend and backend roles, and widens the gender pay gap.

[Guardian]: https://www.theguardian.com/technology/2017/mar/14/tech-women-code-workshops-developer-jobs

## The moral of the story

And to answer the question?

It depends.
But probably yes.
