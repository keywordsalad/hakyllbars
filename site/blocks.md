---
title: Blocks
layout: default
---

Blocks in Hakyllbars delimit and manipulate _blocks_ of template content. Functions define blocks, and Hakyllbars comes with several included.

## Alternating blocks

Hakyllbars supports conditional rendering with _alternating blocks_. The simplest alternating block is the `if` block.

```html
{{-*
{{#if condition}}
  <p>Show me!</p>
{{#end}}
-}}
```

The way block syntax works is by using a function, `if` in this case, to determine whether and how to display a block of content. The `if` function has the following conceptual signature:

```haskell
if :: Condition -> Block -> Value
```

The `if` function chooses based on the truthiness of the condition whether to permit the content of the block. If the condition is falsy, then a falsy block is returned.

_Alternatively_ a different block may be shown if the block returned by `if` is falsy.

```html
{{-*
{{#if condition}}
  <p>Show me!</p>
{{#else}}
  <p>Show me instead!</p>
{{#end}}
-}}
```

The `else` block will permit its contents if the previous block is falsy. These blocks may also permit content based on conditions.

```html
{{-*
{{#if firstCondition}}
  <p>Show me first!</p>
{{#else if secondCondition}}
  <p>Show me second!</p>
{{#else}}
  <p>Show me otherwise!</p>
{{#end}}
}}
```

### Loops

Values containing lists of contexts may be iterated over by using the `for` function.

```html
{{-*
{{#for posts}}
  <h1><a href="{{absUrl}}">{{title}}</a></h1>
  {{teaser}}
{{#end}}
-}}
```

If the values in the loop are empty, then an alternate block may be shown instead.

```html
{{-*
{{#for posts}}
  <h1><a href="{{absUrl}}">{{title}}</a></h1>
  {{teaser}}
{{#else}}
  <p>No posts found.</p>
{{#end}}
-}}
```

## Layout blocks

Blocks may be surrounded completely by a layout template. Layouts may be configured in the context, as in here:

```haskell
layoutField "applyLayout" "path/to/layouts"
```

Then a layout may be created and applied to the following template:

```html
{{-*
<!-- path/to/layouts/post.html -->
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  <h1>{{title}}</h1>
  {{body}}
</body>
</html>

<!-- how-to-apply-a-layout.md -->
--
title: How to apply a layout
--
{{@applyLayout 'post.html'}}
Step 1...
-}}
```

Which interpolates as:

```html
<html>
<head>
  <title>How to apply a layout</title>
</head>
<body>
  <h1>How to apply a layout</h1>
  <p>Step 1...</p>
</body>
</html>
```

### Block-level layouts

Layouts can be applied at the block-level as well:

```html
{{-*
{{#if exampleFigure}}
  {{@figureChrome 'picture-frame.html'}}
  <img src="{{exampleFigure}}">
{{#end}}
-}}
```
