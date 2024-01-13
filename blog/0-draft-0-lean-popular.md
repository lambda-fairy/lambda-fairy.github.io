# Why is Lean popular?

In recent years, the Lean theorem prover, and its flagship mathematics library mathlib, have seen tremendous growth:

![Number of files in mathlib, showing a steady upward trend, ending at over 4000 in January 2024.](/images/2024/lean-popular/lean-number-of-files.png)

But Lean is not the only theorem prover out there.
Why has it caught so much attention where its peers---Agda, Coq, Isabelle, and others---have not?

## Process

Above all else, mathlib's development process is optimized for *speed*.
(Lean itself doesn't move as quickly, but it's comfortable with breaking changes too.)

One factor that enables this speed is that Lean and mathlib are relatively new.
Coq is much older, and has many external users that depend on it.
They don't have permission to change as much as Lean does.

Another factor is that mathlib is a [monorepo].
By developing the entire library in a single repository, we:

-   **Encourage collaboration.**
    Most modern mathematics depends on a wide range of disciplines.
    With a monorepo, contributors are encouraged to reuse what others have built instead of creating it themselves.

-   **Enable sweeping changes.**
    A maintainer can change a core definition and update all its usages at once, without having to coördinate between repositories.

This velocity is not without drawbacks---it can be difficult to work on projects depending on Lean and mathlib, since they change so often.
It's an open question how we can balance those needs while allowing mathlib to grow.

[monorepo]: https://en.wikipedia.org/wiki/Monorepo

## Tools

Lean and mathlib use contemporary software engineering tools: GitHub for hosting, Visual Studio Code as the code editor, and Zulip for community chat.

While Lean's tactics might not be as strong---Coq's ssreflect and Isabelle's sledgehammer both beat them in different ways---the overall editor experience can compensate.

## Foundations

Coq and Lean are similarly based on the [calculus of constructions].
But they disagree in the details.

[calculus of constructions]: https://en.wikipedia.org/wiki/Calculus_of_constructions

## Marketing

This leads into its next point: how Lean presents itself to the broader community.

Unlike other proof assistants, Lean's narrative presents itself as a tool for *all* mathematicians, not only those interested in formalization.
You can see this in Kevin Buzzard's blog, where he talks about various formalization concepts (like [filters] or [division by zero]) in general mathematical terms, with no mention of type theory at all.

This can be also be seen in the recent project to formalize [perfectoid spaces].
Perfectoid spaces are very new and are an active area of research.
By targeting such a novel area, the authors attracted attention from mainstream mathematicians who otherwise wouldn't be interested in formalization.

[filters]: https://xenaproject.wordpress.com/2021/02/18/formalising-mathematics-workshop-5-filters/
[division by zero]: https://xenaproject.wordpress.com/2020/07/05/division-by-zero-in-type-theory-a-faq/
[perfectoid spaces]: https://leanprover-community.github.io/lean-perfectoid-spaces/

## The moral of the story

Time will tell whether its strategy will win out in the end.
Perhaps a better, more friendly theorem prover will appear and take over.

But Lean's approach has worked well so far.
Whether it holds up or not, the community has done a lot to advance formalized mathematics as a whole.

[![Jacaranda tree](/images/2024/jacaranda.jpg)](https://commons.wikimedia.org/wiki/File:Jacaranda_mimosifolia_3994.jpg)
