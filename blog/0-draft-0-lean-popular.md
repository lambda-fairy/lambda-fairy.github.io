# Why is Lean popular?

In recent years, the [Lean theorem prover], and its flagship mathematics library [mathlib], have seen tremendous growth:

[![Number of files in mathlib, showing a steady upward trend, ending at over 4000 in January 2024.](/images/2024/lean-popular/lean-number-of-files.png)](https://leanprover-community.github.io/mathlib_stats.html)

But Lean is not the only theorem prover out there.
Why has it caught so much attention where its peers---Agda, Coq, Isabelle, and others---have not?

[Lean theorem prover]: https://lean-lang.org/
[mathlib]: https://leanprover-community.github.io/

## Lean is new

Lean and mathlib are relatively new.
Agda, Coq, and Isabelle are much older, and have many external users that depend on them.
This means that Lean maintainers can experiment, and make sweeping changes, in a way that these other systems cannot.

This approach is not without drawbacks.
In particular, it can be difficult to work on projects depending on Lean and mathlib, since they change so often.
It's an open question how we can balance those needs while allowing Lean to improve.

## Mathlib is a monorepo

Another factor is that mathlib is a [monorepo]---that is, everyone collaborates on one big repository.

For formalization, the biggest benefit of a monorepo is **reuse**.
A typical proof depends on a web of definitions and lemmas.
In contemporary mathematics, which often bridges many disciplines, this web can be very broad.
With a monorepo, it's easy to draw upon (and contribute to) the code that others have built, instead of having to dig through GitHub---or worse, re-coding it all ourselves.

[monorepo]: https://en.wikipedia.org/wiki/Monorepo

## Lean isn't just for type theorists

Lean's narrative presents itself as a tool for *all* mathematicians, not only those interested in formalization.
You can see this in Kevin Buzzard's blog, where he talks about various formalization concepts (like [filters] or [division by zero]) in general mathematical terms, with no mention of type theory at all.

This contrasts with the approach taken by other proof assistants, like Coq.
While Coq and Lean have similar foundations in type theory, they are [completely different in culture][culture].

Coq users care a lot about type system properties such as how every expression has a unique normal form ("canonicity").
They spend much time exploring systems, like [homotopy type theory], that extend what's possible while respecting these properties.

On the other hand, Lean users see the type system merely as a tool to formalize mathematics.
They're willing to compromise any type theoretic properties (except [soundness]) if it lets them code faster.
Coq's approach might be cleaner in the long run, but Lean's compromises let us do mathematics now.

[filters]: https://xenaproject.wordpress.com/2021/02/18/formalising-mathematics-workshop-5-filters/
[division by zero]: https://xenaproject.wordpress.com/2020/07/05/division-by-zero-in-type-theory-a-faq/
[culture]: https://artagnon.com/computing/coq/leancoq
[homotopy type theory]: https://homotopytypetheory.org/coq/
[soundness]: https://en.wikipedia.org/wiki/Soundness

## Lean has a supportive community

But perhaps the most important factor is how Lean welcomes beginners.

Its [community chat room] is very active, and experienced members take the time to answer questions from newbies.
Indeed, maintainer Patrick Massot calls this out as a strength:

> We can spend ages describing everything that is nice in Lean, and compare it to what we think we know about the other proof assistants.
> But the actual reason why Kevin and I are here is that we came to Gitter (that we used to chat before Zulip) and Johannes Hölzl and especially Mario Carneiro spent days answering all our dumb questions until we started to understand something.
> [We both tried Coq before, and it lacked a Mario.][mario]

[community chat room]: https://leanprover.zulipchat.com/
[mario]: https://leanprover.zulipchat.com/#narrow/stream/113488-general/topic/CICM.202020/near/206517172

[![Jacaranda tree](/images/2024/jacaranda.jpg)](https://commons.wikimedia.org/wiki/File:Jacaranda_mimosifolia_3994.jpg)
