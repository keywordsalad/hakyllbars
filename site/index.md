---
title: Why another templating language?
layout: default
---

[hakyll][]'s own templating language is very simple. It doesn't permit functions or provide a means to extend its syntax. Simple templates are easy to define and include, but I want something more expressive.

I created my own template language that allows me to specify complex includes, layouts, and define functions. The syntax is inspired by [HandlebarsJS](https://handlebarsjs.com), so I call it _hakyllbars_!

_See hakyll's own template docs [here](https://jaspervdj.be/hakyll/tutorials/04-compilers.html)._

**Hakyllbars supports the following:**

* [Variables]({{getUrl 'variables.md'}})
* [Helper functions]({{getUrl 'helper-functions.md'}})
* [Layouts]({{getUrl 'layouts.md'}})
* [Control flow]({{getUrl 'control-flow.md'}})

## Getting started

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
      HB.gitFields providerDirectory gitWebUrl -- Sets up the git fields
        <> HB.dateFields dateConfig            -- Sets up the date fields
        <> HB.defaultFields host siteRoot      -- Using the default fields is very recommended
    providerDirectory = "site" -- where your Hakyll files live
    gitWebUrl = "https://github.com/keywordsalad/hakyllbars/tree"
    host = "https://keywordsalad.github.io"
    siteRoot = "/hakyllbars"
```

[hakyll]: https://jaspervdj.be/hakyll/
