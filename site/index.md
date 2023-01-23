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

### Variables

```html
{{-*
<h1>Hello, {{name}}!</h1>

<p>This is how to interpolate a {{variable}}.</p>
-}}
```

Any variable that can be stringified may be interpolated!

Variables may be declared in markdown frontmatter:

```markdown
{{-*
---
title: This is my website
updated: 2023-01-21
---

# {{title}}

Last updated on {{updated}}.
-}}
```

### Helper functions

```html
{{-*
{{partial 'header'}}
-}}
```

Variables that are functions may be applied to one or more arguments. As in Haskell, arguments are separated by whitespace.

Functions may be applied and then _piped forward_ as arguments to other expressions by using the `|` operator.

```html
{{-*
<a href="{{getAbsUrl 'sitemap.xml' | htmlEscape}}">Sitemap</a>
-}}
```

This is equivalent to writing:

```html
{{-*
<a href="{{htmlEscape (getAbsUrl 'sitemap.xml')}}">Sitemap</a>
-}}
```

Helper functions are also used to define syntax, as in [conditionals](#conditions) and [loops](#loops).

#### Hakyllbars defines a number of helper functions by default!

`dateAs`
: todo

`escapeHtml`
: todo

`getAbsUrl`
: todo

`getUrl`
: todo

`linkedTitle`
: todo

### Layouts

The file `example.md` declares `page` as its layout using the `appleLayout` helper function:

```markdown
{{-*
---
title: This is a page
---
{{@applyLayout 'page'}}
This is a page with a layout.
-}}
```

The `page` layout is defined in `_layouts/page.html`:

```html
{{-*
<!doctype html>
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  <h1>{{title}}</h1>
  {{body}}
</body>
</html>
-}}
```

The content from `example.md` is interpolated into `_layouts/page.html` like this:

```html
<!doctype html>
<html>
<head>
  <title>This is a page</title>
</head>
<body>
  <h1>This is a page</h1>
  <p>This is a page with a layout.</p>
</body>
</html>
```

### Conditions

Conditional expressions are supported by the `if` helper function. They permit a block of content if a given condition is _truthy_:

```markdown
{{-*
{{#if author}}
  This page was written by {{author}}.
{{#end}}
-}}
```

Default content may be provided:

```markdown
{{-*
{{#if published}}
  This page was published on {{published}}.
{{#else}}
  This page is a draft.
{{#end}}
-}}
```

Additionally, multiple conditions may be chained in sequence:

```markdown
{{-*
{{#if updated}}
  This page was updated on {{updated}}.
{{#else if published}}
  This page was published on {{published}}.
{{#else}}
  This page is a draft.
{{#end}}
-}}
```

### Loops

Lists of items may be iterated over using the `for` helper function. Each item's attributes become available in the scope of the associated block.

```html
{{-*
<h1>Posts</h1>
<ul>
  {{#for posts}}
    <li>
      <strong><a href="{{url}}">{{title}}</a></strong>
      {{#if published}}
        Published on {{published}}.
      {{#else}}
        This post is a draft.
      {{#end}}
    <li>
  {{#end}}
</ul>
-}}
```

If a list is empty, a default block may be provided:

```html
{{-*
<ul>
  {{#for posts}}
    ...
  {{#else}}
    <li>There are no posts here.</li>
  {{#end}}
</ul>
-}}
```
