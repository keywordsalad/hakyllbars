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
