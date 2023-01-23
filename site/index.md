---
title: Why another templating language?
layout: default
---

Hakyll's own templating language is very simple. It doesn't permit functions or provide a means to extend its syntax. Simple templates are easy to define and include, but I want something more expressive.

I created my own template language that allows me to specify complex includes, layouts, and define functions. The syntax is inspired by [HandlebarsJS](https://handlebarsjs.com), so I call it Hakyllbars!

_See Hakyll's own template docs [here](https://jaspervdj.be/hakyll/tutorials/04-compilers.html)._

## Things I want in a templating language

* [Variables](#variables)
* [Helper functions](#helper-functions)
* [Layouts](#layouts)
* [Conditions](#conditions)
* [Loops](#loops)

{{section 'variables'}}
{{section 'helper-functions'}}
{{section 'layouts'}}
{{section 'conditions'}}
{{section 'loops'}}
