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
