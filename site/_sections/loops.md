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
