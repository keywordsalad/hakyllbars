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
