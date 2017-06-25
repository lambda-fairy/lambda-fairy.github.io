---
title: Splitting a list into chunks with unfoldr
tags: code, haskell
---

I'm quite fond of the [`unfoldr`][unfoldr] function in Haskell. It doesn't seem well known, which is a shame because I've often found it useful.

One of these uses is in splitting a list into chunks of a specified length. Here are some examples of what I'm going for:

```haskell
>>> chunks 1 ['a'..'l']
["a","b","c","d","e","f","g","h","i","j","k","l"]

>>> chunks 2 ['a'..'l']
["ab","cd","ef","gh","ij","kl"]

>>> chunks 3 ['a'..'l']
["abc","def","ghi","jkl"]

>>> chunks 4 ['a'..'l']
["abcd","efgh","ijkl"]

>>> chunks 5 ['a'..'l']
["abcde","fghij","kl"]
```

And here's the definition:

```haskell
chunks :: Int -> [a] -> [[a]]
chunks = unfoldr $ \xs ->
    case xs of
        -- If there are no elements left, stop iteration
        [] -> Nothing
        -- Otherwise, split off n elements and yield them
        _ -> Just $ splitAt n xs
```

[unfoldr]: https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-List.html#v:unfoldr
