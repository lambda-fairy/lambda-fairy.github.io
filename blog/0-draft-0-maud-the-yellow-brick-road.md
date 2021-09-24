# Maud: Beyond 1.0

<aside>
<p>
<a href="https://maud.lambda.xyz">Maud</a> is a macro for writing HTML.
Compared to other template engines, Maud offers better type checking and lower runtime overhead.
</p>
</aside>

Maud is old.

*Very* old.

When Maud 0.1 was released, over 6 years ago, Rust itself was still in alpha.
Procedural macros were unstable---not that it mattered, because *everything* was unstable.

Each `rustup update` was filled with anticipation.
What's going to break this time?
Import syntax?
The `Writer` trait renamed to `Write`?[^strange]

[^strange]: I was a strange kid.

I didn't realize it back then, but that was a great time to get started.
Rust was new and exciting.[^exciting]
I could say "blazing fast" and not feel like an idiot.
Maud could be the most efficient and ergonomic and easy-to-learn compile-time template engine, because it was the *only* compile-time template engine.
It was a whole new design space, and I was first.

[^exciting]: It's still exciting, but it's no longer new 🙂

But it's not 2015 anymore.
Proc macros are stable and easy now, and people have realized that!
There are [many][askama] [many][sailfish] [many][horrorshow] great choices around, all with their own particular strengths.
Even with my bias, I don't know if I'd recommend Maud today.

[askama]: https://crates.io/crates/askama
[sailfish]: https://sailfish.netlify.app/en/
[horrorshow]: https://crates.io/crates/horrorshow

And, to be honest, I'm not sure if the *concept* of "compile-time template engine" is that valuable anymore.
Macro magic might [look great on benchmarks], but at what cost?
Generating HTML is not your bottleneck.
But compile times, and the cost of learning new syntax, may very well be.

[look great on benchmarks]: /blog/maud-is-fast/

Quoth Martin Odersky, the creator of Scala:

> bla bla

He's talking about Scala's type system, but that complaint could apply just as well to macros.

## Business time

But not all hope is lost.

In business strategy, there's a technique called [SWOT analysis][swot]: Strengths, Weaknesses, Opportunities, and Threats.
We've discussed at length the Weaknesses and Threats; what about the other two?

[swot]: https://en.wikipedia.org/wiki/SWOT_analysis

### **Strength:** Small-scale development

This is a complete Maud (and Rouille) app:

```rust
use maud::html;
use rouille::Response;

fn main() {
    rouille::start_server("0.0.0.0:80", move |request| {
        Response::html(html! {
            p { "Hello, world!" }
        })
    });
}
```

None of the other libraries are this short.
Askama and Sailfish need a `struct` declaration and a separate template file.
Even Horrorshow needs an `.into_string()` call!

I found this comment on Reddit:

> TODO

I think this captures why people like the library.
At some point, you're going to split code into modules, you're going to bring in other people, and the magic might wear off then.
But until that happens, you can inline everything and have it fit in a single screen.
And if your app never gets big---it can stay that way.

### **Opportunity:** Security by design

Rust is known for its commitment to security by design.
After all, its primary goal is to fix memory safety: the cause of 2 in 3 zero-days in large C and C++ apps.
And it does that not by layering mitigations, but through a type system that enforces it upfront.

But this doesn't have to stop at memory.
In particular, with web apps, there's another big security problem: *cross-site scripting*, or *XSS*.

Here's an example.
Suppose that your site has a guest book where visitors can leave a friendly message:

TODO image

Whatever text they submit would show up on the page:

TODO image

```html
<p>What a nice website!</p>
```

But what if someone submits this instead?

TODO image

```html
<p><script>alert("haha! imma steal all your data :3");</script></p>
```

Oh no!

The problem is that HTML doesn't distinguish between code the *developer* wrote and code the *user* wrote.
If we mix the two, then malicious users can take the role of the developer, and change the page to their liking.

To mitigate this, most template engines have a basic form of escaping: they defang `<` into `&lt;`, `&` into `&amp;`, and so on, for every piece of text substituted into the page.

But while this works most of the time, it doesn't work everywhere.

-

We can argue that people who make these mistakes just "don't know web security", and that they should read the [OWASP guide] or something.
But that's precisely the argument that C and C++ fans use to disparage Rust.
It shouldn't take learning 4500 words of rules to write an <abbr title="cross-site-scripting">XSS</abbr>-safe app, just as it shouldn't take learning [100,000 words][C++ guidelines] to write a memory-safe one.
And while context-aware escaping can be complex, it's a lot easier than the borrow checker; and like Rust itself, there's a lot we can do to make it easier to learn.

[OWASP guide]: https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html
[C++ guidelines]: https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines

## Security in the small
