---
title: Why another templating language?
layout: default
---

[hakyll][]'s own templating language is very simple. It doesn't permit functions or provide a means to extend its syntax. Simple templates are easy to define and include, but I want something more expressive.

I created my own template language that allows me to specify complex includes, layouts, and define functions. The syntax is inspired by [HandlebarsJS](https://handlebarsjs.com), so I call it _hakyllbars_!

_See hakyll's own template docs [here](https://jaspervdj.be/hakyll/tutorials/04-compilers.html)._

## Things I want in a templating language

* [Variables](#variables)
* [Helper functions](#helper-functions)
* [Layouts](#layouts)
* [Conditions](#conditions)
* [Loops](#loops)

{{section 'variables'}}

### Helper functions

{{partial 'helper-functions'}}

**[hakyllbars defines a number of helper functions by default!]({{getUrl 'helper-functions.md'}})**

{{section 'layouts'}}
{{section 'conditions'}}
{{section 'loops'}}

[hakyll]: https://jaspervdj.be/hakyll/
