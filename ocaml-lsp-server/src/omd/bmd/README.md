This directory contains expressions that **"break"** Markdown.

For instance:
```
   * Starting a list with 3 spaces and a star.
   * Continuing the list.
  * 2 spaces and a star: are we still in the same list?
```

On Github it renders like this:

   * Starting a list with 3 spaces and a star.
   * Continuing the list.
  * 2 spaces and a star: are we still in the same list?


So you can see that on Github, the star with preceded by less spaces starts an **inner** list, which is kind of very wrong...
Pandoc considers that the 3rd bullet starts the 3rd element of the unique list, which is not so right either.
I'm not blaming those tools, but rather the language. (On second thoughts, I might endup blaming the tools rather than the language.)

