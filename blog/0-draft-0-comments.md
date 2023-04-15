# Comments

```rust
if !literal.ends_with('\n') {
    // Syntect expects a trailing newline
    literal.push('\n');
}
```

Comments are great.
When there's a bit of context that isn't obvious from the code, comments can fill in the gaps.

But, like anything related to computers, they can be a little cursed.

![A blue-haired girl swallowing a cat.](/images/2023/comments/mio.jpg)

Let me elaborate.

## Comments can be nested

In Haskell, a multi-line comment starts with `{-` and ends with `-}`.

```haskell
{- This is a comment. -}
```

This syntax can be nested.

```haskell
{-
    This is a comment.
        {- This is also a comment. -}
    This is still a comment.
-}
```

If you don't consider this surprising, congrats!
Run away while you still can.

See, in a traditional language, comments are handled in the _tokenizer_: a simple, initial pass which splits the source code into little pieces (tokens).
For simplicity, the tokenizer uses a regular grammar.
I won't go into the details here, but in short, this means **no nesting**.

But Haskell is not a traditional language.
Many of its design choices -- from its custom operators to its lazy evaluation -- favor expressiveness over being easy to compile.

And that's why C, which trades off in the opposite direction, does _not_ support nested comments.

```c
/*
    This is a comment.
        /* This is also a comment. */
    This is no longer part of a comment, and will cause a syntax error.
*/
```

Whether a language supports nested comments speaks to this fundamental trade-off.
Do the designers care more about being *easy to use*, or *easy to implement*?

## Comments are control flow

## Comments might not actually exist

Enter [m4].

m4, like all old Unix tools, is based on the manipulation of byte streams.
(_Not_ text, mind you.
Unix knows nothing about that.)

Its fundamental -- and only -- operation is to replace one byte sequence with another.

If all you have is a hammer, everything looks like a nail.
If all you have is find-and-replace, then a "comment" is simply a macro that replaces its argument with nothing.

While m4 does have a built-in [`dnl` (Discard to Next Line)][dnl], it's not fundamental.
You can define your own macro that does the same thing:

```m4
define(mycomment,)

mycomment(This is a comment! This is removed from the output!)

This is not a comment. This is included in the output.
```

[m4]: https://en.wikipedia.org/wiki/M4_(computer_language)
[dnl]: https://www.gnu.org/software/m4/manual/html_node/Dnl.html
