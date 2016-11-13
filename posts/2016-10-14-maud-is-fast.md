---
title: The fastest template engine in the West
tags: code, rust
---

Lately I've been working on [Maud], an HTML template library for Rust. One of the features that make it special is that it works at *compile time*. That is, your templates are compiled to plain Rust code, and type-checked and optimized with the rest of your app.

Among other benefits, this design makes the library *fast*. Like, really, really fast. To get an idea of how fast it is, take a look at this graph:

[![Graph of render times for different template engines](/images/2016/maud-is-fast.svg)](/images/2016/maud-is-fast.svg)

That's right. Maud is 69 times faster than Handlebars.

I know what you're thinking:

- "Omigosh, did he just say the *sex number*?"
- "Why is Maud so much faster than the other engines?"
- "Wow... Lambda-san is so attractive, I feel dizzy just thinking about him."

For the sake of professionalism, I will only answer the second question.

[Maud]: https://github.com/lfairy/maud

Maud is fast, because it **does as little work as possible at runtime**. For example, the following template:

```rust
html! {
    p { "Hi, " (name) "!" }
}
```

expands to this Rust code:

```rust
{
    // Allocate a `String` to hold the resulting markup
    let mut __maud_writer = ::std::string::String::with_capacity(25usize);
    __maud_writer.push_str("<p>Hi, ");
    {
        // Append the value of `name` to the result, while escaping any
        // HTML special characters (e.g. `<` → `&lt;`)
        use ::maud::RenderOnce;
        name.render_once(&mut __maud_writer)
    };
    __maud_writer.push_str("!</p>");
    // Mark the result as valid HTML, so we don't escape it again
    ::maud::PreEscaped(__maud_writer)
}
```

In other words, the resulting code does little more than building a string.

Compare this to other---more dynamic---template engines, which may encode the input data as JSON, or look up the name of the template in a global registry, or do a variety of other things. These engines are effectively language [interpreters], with all the pros and cons of working that way. And as the benchmark above shows, one of these cons is reduced performance.

[interpreters]: https://en.wikipedia.org/wiki/Interpreter_(computing)

That's not to say that these dynamic approaches aren't useful. Speed isn't the only factor in choosing a template engine; and to be fair, when an average request takes hundreds of milliseconds already, a *micro*second difference doesn't matter that much. Other engines also let you edit a template without re-compiling the app, and their syntax can feel more familiar to users of other languages.

What I do want to show, then, is how design decisions that seem minor at first can have a big impact down the road. I've added a bunch of optimizations over the years (some of them stolen from [Horrorshow]), but none of them have affected performance that much. The largest difference is in static vs dynamic; the rest is details.

[Horrorshow]: https://github.com/Stebalien/horrorshow-rs
