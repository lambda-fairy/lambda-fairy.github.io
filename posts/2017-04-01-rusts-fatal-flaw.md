---
title: Rust's fatal flaw
tags: fluff, rust
---

I like [Rust]. It's named after an awesome parasitic fungus, it's developed by a non-profit (Mozilla), and its logo gives it a nice steampunk aesthetic. It's also great fodder for deep, thoughtful think pieces. Like this one.

Despite these advantages, Rust does have a fatal flaw. Now, having fatal flaws isn't a deal breaker in itself. After all, Haskell still has no way to declare two structs with the same field name, and yet it regularly hits the front page of Hacker News.

But this time, it's different. This flaw touches on a feature much more fundamental than struct declarations. If left unfixed, it threatens to lock out all but the most dedicated users from the language.

The flaw involves Rust's distinction between *owned* and *borrowed* types. Owned values wrap a resource; this resource is de-allocated automatically when the value falls out of scope. Compare this to a borrowed value, which may point to a resource but does not take responsibility for de-allocating it.

Borrowed values often have fewer capabilities than their owned counterparts, but are in turn easier to pass around. For example, both an owned `String` and a borrowed `&str` point to UTF-8--encoded text, but only `String` lets you mutate and grow the underlying buffer. On the other hand, since `&str` handles do not manage the buffer themselves, they can be copied cheaply without risking a [double free].

This structure is intuitive and obvious, and a clear improvement over that of other systems languages like C++ or JavaScript. The flaw, then, is not in the basic idea, nor the execution, but in the names of the types themselves.

As mentioned above, we have the owned `String` and the borrowed `&str`. But with arrays, the developers have opted for the confusing `Vec<T>` and `&[T]` instead. Going through the names for common owned and borrowed types, we realize that they do not follow a consistent pattern:

| Owned | Borrowed |
| :-- | :-- |
| `String` | `str` |
| `PathBuf` | `Path` |
| `OsString` | `OsStr` |
| `Vec<T>` | `[T]` |

Uppercase vs lowercase, two different suffixes, and a different set of symbols altogether. How can a newcomer be expected to learn this naming scheme? If the Rust developers really [care about ergonomics][ergonomics], then this is a good place to start.

Luckily, the author has a solution. Suppose that the names of these types were changed to the following:

| Owned | Borrowed |
| :-- | :-- |
| `String` | `Str` |
| `Pathing` | `Path` |
| `OsString` | `OsStr` |
| `Slicing<T>` | `Slic<T>` |

This scheme is easy to learn: just add `-ing` for the owned type. (We shall refer to this as *Dutch notation*, for reasons elaborated on later.) As an added benefit, the `Slic` name alludes to the efficiency of Rust's zero-cost abstractions. Very *slic*.

That raises the question, though: why the choice of an `-ing` suffix to represent ownership? The answer should be familiar to anyone with a passing interest in finance. The International Netherlands Group, or [ING] for short, is a multinational banking and financial services corporation headquartered in Amsterdam. Since a bank is in the business of tracking ownership, it feels appropriate to use the name of one in the context of Rust.

Funnily enough, this idea provides us with an explanation for the original naming scheme. String is produced by the textile industry, which is largely in private hands. It would make sense for ING to invest in its production. Most (foot)paths, however, are maintained by the state; they are shielded---or *buffered*---from market forces. And vectors are a concept from mathematics, a field known for its use of arcane symbols.

If neither Dutch notation nor desperate post-hoc rationalizing appeal to you, then there is one final alternative. Perhaps, when the workers of the world rise up and seize the means of production, we can abolish the idea of ownership altogether. I think that would be a great time to release Rust 2.0.

[Rust]: https://www.rust-lang.org
[double free]: https://www.owasp.org/index.php/Double_Free
[ergonomics]: https://blog.rust-lang.org/2017/03/02/lang-ergonomics.html
[ING]: https://en.wikipedia.org/wiki/ING_Group
