---
title: Layouts
layout: default
created: 2023-02-26T16:08:40-0800
---

Hakyllbars supports several helpers to enable layouts within your pages.

## `@applyLayout`

Layouts may be applied using the `@applyLayout` helper. The item to which the template is applied is make available to
the template as the `item` variable.

```html
{{-*
<!-- _layouts/main.html -->
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  {{item}}
</body>
</html>

<!-- page.md -->
---
title: This is my page
___
{{@applyLayout 'main.html'}}

Your content here.
-}}
```

## Layouts applied by metadata

Layouts may also be driven by metadata by using the following trick:

```html
{{-*
<!-- _layouts/main.html -->
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  {{item}}
</body>
</html>

<!-- _layouts/from-context.html -->
{{#if layout-}}
  {{@applyLayout layout-}}
  {{body-}}
{{#else-}}
  {{body-}}
{{#end-}}

<!-- page.md -->
---
title: This is my page
layout: main
___
Your content here.
*}}
```

And then when compiling your pages, use `HB.applyTemplate "_layouts/from-context.html"`:

```haskell
-- Main.hs
compile do
  getResourceBody >>= HB.applyTemplates do
    HB.applyContext context
    HB.applyContent
    HB.applyTemplate "_layouts/from-context.html"
```

## Compiling layouts

Layouts should live in the `_layouts/` folder within your site source. Layouts must also be compiled before they may be
used, and this is accomplished with the following code:

```haskell
templates :: Rules Dependency
templates = do
  match "_layouts/**" do
    compile $
      getResourceBody
        >>= HB.compileTemplateItem
        >>= makeItem
  makePatternDependency templatePattern

main :: IO ()
main = do
  templatesDependency <- templates
  rulesExtraDependencies [templatesDependency] do
    match "*.md" do
      route $ setExtension "html"
      compile do
        getResourceBody >>= HB.applyTemplates do
          HB.applyContext context
          HB.applyContent
          HB.applyTemplate "_layouts/from-context.html"
```
