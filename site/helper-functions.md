---
title: Helper functions
layout: default
created: 2023-01-28T09:53:41-0800
---

{{partial 'helper-functions'}}

## Provided helper functions

* [`dateAs`](#dateas)
* [`escapeHtml`](#escapehtml)
* [`getAbsUrl`](#getabsurl)
* [`getUrl`](#geturl)
* [`linkedTitle`](#linkedtitle)
* [`put`](#put)
* [`get`](#get)
* [`pubBlock`](#putblock)
* [`addBlock`](#addblock)

### `dateAs`

Use this helper function with the following date format variables in this syntax:

```haskell
dateVariable | dateAs formatVariable
```

| Variable    | Default value              |
|:------------|:---------------------------|
| `longDate`  | `"%B %e, %Y %l:%M %P %EZ"` |
| `shortDate` | `"%B %e, %Y"`              |
| `timeOnly`  | `"%l:%M %p %EZ"`           |
| `robotDate` | `"%Y-%m-%d"`               |
| `robotTime` | `"%Y-%m-%dT%H:%M:%S%Ez"`   |

#### Live examples

| Expression                            | Result                           |
|:--------------------------------------|:---------------------------------|
| `{{*{{created | dateAs longDate}}}}`  | `{{created | dateAs longDate}}`  |
| `{{*{{created | dateAs shortDate}}}}` | `{{created | dateAs shortDate}}` |
| `{{*{{created | dateAs timeOnly}}}}`  | `{{created | dateAs timeOnly}}`  |
| `{{*{{created | dateAs robotDate}}}}` | `{{created | dateAs robotDate}}` |
| `{{*{{created | dateAs robotTime}}}}` | `{{created | dateAs robotTime}}` |

### `escapeHtml`

Stringifies and escapes its argument for interpolation into HTML. Use with the following syntax:

```html
{{-*
<img alt="{{imgAlt | escapeHtml}}" title="{{imgTitle | escapeHtml}}" src="...">
-}}
```

#### Live examples

| Expression                                       | Result                                      |
|:-------------------------------------------------|:--------------------------------------------|
| `{{*{{'<em>Hello, World!</em>' | escapeHtml}}}}` | `{{'<em>Hello, World!</em>' | escapeHtml}}` |

### `getAbsUrl`

Gets the absolute URL to a given item file path relative to the `host` and `siteRoot` variables using the form `{{*{{host}}/{{siteRoot}}/{{getUrl itemPath}}}}`.

```html
{{-*
<!-- given:
host: "https://website.com"
siteRoot: blog
-->
<a href="{{getAbsUrl 'recipes.md'}}">Recipes</a>

<!-- interpolates as... -->
<a href="https://website.com/blog/recipes.html">Recipes</a>
-}}
```

#### Live examples

| Expression              | Result |
|:------------------------|:-------|
| `{{*{{getAbsUrl id}}}}`

### `getUrl`

Gets the URL to a given item file path relative to the `siteRoot` variable using the form `{{*{{siteRoot}}/{{getUrl itemPath}}}}`.

```html
{{-*
<!-- given:
siteRoot: blog
-->
<a href="{{getAbsUrl 'recipes.html'}}">Recipes</a>

<!-- interpolates as... -->
<a href="/blog/recipes.html">Recipes</a>
-}}
```

### `linkedTitle`

Creates a link to a given item file path with its title as the value.

```html
{{-*
<!-- given:
title: How to link a blog post
date: 2023-01-23
-->
{{linkedTitle 'blog/how-to-link.md'}}

<!-- interpolates as... -->
<a href="/blog/2023/01/23/how-to-link/index.html">How to link a blog post</a>
-}}
```

### `put`

Puts a variable into the current context. The variable will be propagated to any following included templates.

```html
{{-*
{{put "fruit" "banana"}}
Fruit is: _{{fruit}}_

<!-- interpolates as... -->
<p>Fruit is: <em>banana</em></p>
}}
```

### `get`

Gets a variable by name.

```html
{{-*
<!-- given:
fruit: banana
variable: fruit
-->
Variable _{{variable}}_ is _{{get "variable"}}_.

<!-- interpolates as... -->
<p>Variable <em>fruit</em> is <em>banana</em>.</p>
}}
```

### `putBlock`

Puts an inline template into a variable. Note that this helper function requires the `#` prefix.

```html
{{-*
<!-- given:
fruit: banana
-->
{{#putBlock "inlinePartial"}}
  This content will be captured here, with a fruit: _{{fruit}}_.
{{#end}}
{{inlinePartial}}

<!-- interpolates as... -->
<p>This content will be captured here, with a fruit: <em>banana</em>.
}}
```

### `addBlock`

Creates a list of inline templates or adds a template to an existing list. Note that this helper function requires the `#` prefix.

```html
{{-*
<!-- given:
firstFruits: apples
secondFruits: oranges
-->
{{#addBlock "fruits"}}
  This is the first fruit: _{{firstFruit}}_.
{{#end}}
{{#addBlock "fruits"}}
  This is the second fruit: _{{secondFruit}}_.
{{#end}}
{{fruits}}

<!-- interpolates as... -->
<p>This is the first fruit: <em>apples</em>.</p>
<p>This is the second fruit: <em>oranges</em>.</p>
-}}
```
