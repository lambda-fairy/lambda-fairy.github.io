# New website: Frontend

*This is part 2 of a trilogy.
You might like to read [part 1].*

[part 1]: /blog/new-website-part-1/

In this blog post, I'll point out some of the features---both visible and less visible---in the user-facing part of the redesign.

## Black is the new black

This text might appear black, but it's actually a very dark gray.
The tweak helps take the edge off the extreme contrast that black-on-white would otherwise have.
You can see the [Material Design] guidelines make the same suggestion.

[Material Design]: https://material.io/design/color/text-legibility.html#text-backgrounds

Accessibility advocates might raise concerns about the readability of such grays.
A [2016 WIRED article] bemoans the rise of gray text, and how it has made the web unusable for its author.

But those concerns are for sites which take the idea too far.
A pinch of gray is unlikely to make the text unreadable.
Indeed, the "87% black" used here has a contrast ratio of [15.9:1]---much higher than the 7:1 set by WCAG AAA.

[2016 WIRED article]: https://www.wired.com/2016/10/how-the-web-became-unreadable/
[15.9:1]: https://webaim.org/resources/contrastchecker/?fcolor=222222&bcolor=FFFFFF

## A variety of weights

I use [Libre Franklin] for the font, which provides many options for weight, or thickness.
The design makes good use of this flexibility:

[Libre Franklin]: https://fonts.google.com/specimen/Libre+Franklin

- For the **header**, a thick black establishes hierarchy.
- **Inline links** are given medium weight, so that they stand out from surrounding text but not as much as bold.
- Similarly, the **date indicator** has medium weight to compensate for its smaller size.

Libre Franklin is a [variable font], so it offers hundreds of weights beyond the standard nine.
I haven't found a use for these "in-between" weights yet, but it's definitely something worth exploring in the future.

[variable font]: https://web.dev/variable-fonts/

## Sexy dots

On the [home page], each title is connected to its post date by a row of dots.
These are not images; they're built from pure CSS.

[home page]: /

Thank you to Rúnar Berg, from whose [Stack Overflow answer] I drew heavily.

[Stack Overflow answer]: https://stackoverflow.com/a/28097029/617159

## Responsive design without `@media` queries

Traditionally, to support both mobile and desktop form factors, designers would write two versions of the stylesheet---one for large screens, and one for small:

```css
:root {
    font-size: 20px;
}

@media (max-width: 800px) {
    /* Use a smaller font on mobile */
    :root {
        font-size: 16px;
    }
}
```

While this works, it creates a sudden jump in the boundary between the two sizes.
It's also more effort to keep the two versions in sync.

Modern CSS provides an alternative approach.
With viewport units and the `clamp` function, you can [interpolate] between the large and small designs, creating a seamless transition between them:

[interpolate]: https://css-tricks.com/min-max-and-clamp-are-css-magic/

```css
:root {
    /* Set font size to 4% of screen width, with a minimum
       of 16px and maximum of 20px */
    font-size: clamp(16px, 4vw, 20px);
}
```

My website uses this technique extensively.
If you're on desktop, resize the browser window.
Watch the layout shrink and grow to the available space.

## Keyboard accessibility

When you use <kbd>Tab</kbd> to navigate through the page, the links will pop out as though selected via mouse.
This is implemented by the CSS [`:focus`] selector.

[`:focus`]: https://developer.mozilla.org/en-US/docs/Web/CSS/:focus

It's a minor detail, but one that many designers overlook.

## Peek-a-boo!

Hover (or long-press) on the "lambda fairy" at the top of the page.
Surprise!

This was my favorite feature of my old site, and I'm proud to have it on the redesign too 🙂

## ... and many more

This is far from an extensive list.
There are many other details that I don't have space to cover, such as:

- Using `contain` to disable margin collapse;
- Setting a fallback background color behind the main image;
- Reducing the `line-height` of super- and subscripts;
- Footnote styles.

Please let me know if you'd like me to deep dive into any of these!

In my next---and hopefully final---post, I'll walk through how the site is built and deployed.
