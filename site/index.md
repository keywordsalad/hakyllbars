---
title: Why another templating language?
layout: default
---

[hakyll][]'s own templating language is very simple. It doesn't permit functions or provide a means to extend its syntax. Simple templates are easy to define and include, but I want something more expressive.

I created my own template language that allows me to specify complex includes, layouts, and define functions. The syntax is inspired by [HandlebarsJS](https://handlebarsjs.com), so I call it _hakyllbars_!

_See hakyll's own template docs [here](https://jaspervdj.be/hakyll/tutorials/04-compilers.html)._

## What are hakyllbars?

Hakyllbars are double curly-braces `\{{` and `\}}` which can be used to interpolate expressions into templates.

**Special forms of curly braces are capable of different things.**

There are a set of default **[variables]({{getUrl 'variables.md'}})** and **[helper functions]({{getUrl 'helper-functions.md'}})** available for use with bare `\{{` and `\}}`.

**[Layouts]({{getUrl 'layouts.md'}})** are applied by using `\{{@` and `\}}`, which captures all content following the closing `\}}` and passes it as a syntax block to the function between the hakyllbars:

```{{*
{{@applyLayout "blog-post.html"}}
This will get the blog post treatment.
}}```

**[Control flow]({{getUrl 'control-flow.md'}})** is applied by using `\{{#` and `\}}`, which captures all content up to a following `\{{#end\}}` and passes it as a syntax block to the function between the opening hakyllbars.

```{{*
{{#if published}}
  This article was published on {{published}} by {{author}}.
{{#end}}

{{#for posts}}
  {{title}} - {{url}}
{{#else}}
  No posts found.
{{#end}}
}}```

## Getting started

Here's some basic boilerplate to leverage Hakyllbars in your site:

```haskell
import Hakyllbars as HB
import Data.Time

main = do
  -- Sets up configuration for the date fields
  time <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
  let dateConfig = HB.defaultDateConfigWith defaultTimeLocale time

  match "*.md" do
    route $ setExtension "html"
    -- This is where Hakyllbars is applied
    compile do
      getResourceBody >>= HB.applyTemplates do
        -- Sets the root context used in the templates
        HB.applyContext context
        -- Applies the item content as a template
        HB.applyContent

  where
    context dateConfig =
      -- Sets up the git fields
      HB.gitFields providerDirectory gitWebUrl
        -- Sets up the date fields
        <> HB.dateFields dateConfig
        -- Using the default fields is very recommended
        <> HB.defaultFields host siteRoot

    providerDirectory = "site" -- where your Hakyll files live
    gitWebUrl = "https://github.com/keywordsalad/hakyllbars/tree"
    host = "https://keywordsalad.github.io"
    siteRoot = "/hakyllbars"
```

[hakyll]: https://jaspervdj.be/hakyll/
