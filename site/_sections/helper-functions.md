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
: Use this helper function with the following date format variables in this syntax:

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

`escapeHtml`
: Stringifies and escapes its argument for interpolation into HTML. Use with the following syntax:

    ```html
    {{-*
    <img alt="{{imgAlt | escapeHtml}}" title="{{imgTitle | escapeHtml}}" src="...">
    -}}
    ```

`getAbsUrl`
: Gets the absolute URL to a given item file path. This helper function works with the `host` and `siteRoot` variables to produce absolute URLs using the form `{{*{{host}}/{{siteRoot}}/{{getUrl itemPath}}}}`.

    ```html
    {{-*
    <!-- given:
      host = "https://website.com"
      siteRoot = "blog"
    -->

    <a href="{{getAbsUrl 'recipes.html'}}">Recipes</a>

    <!-- interpolates as... -->
    <a href="https://website.com/blog/recipes.html">Recipes</a>
    -}}
    ```

`getUrl`
: Gets the absolute URL to a given item file path without the `host`. This helper function works with the `siteRoot` variable to produce absolute URLs using the form `{{*{{siteRoot}}/{{getUrl itemPath}}}}`.

`linkedTitle`
: todo
