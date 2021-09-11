# New website: Frontend

*This is part 2 of a trilogy.
You might like to read [part 1].*

[part 1]: /blog/new-website-part-1/

In this blog post, I'll point out the features---both visible and less visible---in the user-facing part of the redesign.

## Black is the new black

This text might appear black, but it's actually a dark gray.
The tweak adds a sense of depth to the piece.
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
The design makes full use of this flexibility:

[Libre Franklin]: https://fonts.google.com/specimen/Libre+Franklin

- For the **header**, a thick black establishes hierarchy.
- **Inline links** are given medium weight, so that they stand out from surrounding text but not as much as bold.
- Similarly, the **date indicator** has medium weight to compensate for its smaller size.

## Stable line heights with `<sup>` and `<sub>`

## Sexy dots

## Keyboard accessibility

When you use <kbd>Tab</kbd> to navigate through the page, the links will pop out as though selected via mouse.
This is implemented by the CSS [`:focus`] selector.

[`:focus`]: https://developer.mozilla.org/en-US/docs/Web/CSS/:focus

It's a minor detail, but one that many designers overlook.

## Responsive design without `@media` queries

## Peek-a-boo!

## ... and many more
