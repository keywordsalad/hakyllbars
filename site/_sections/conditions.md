### Conditions

Conditional expressions are supported by the `if` helper function. They permit a block of content if a given condition is _truthy_:

```markdown
{{-*
{{#if author}}
  This page was written by {{author}}.
{{#end}}
-}}
```

Default content may be provided:

```markdown
{{-*
{{#if published}}
  This page was published on {{published}}.
{{#else}}
  This page is a draft.
{{#end}}
-}}
```

Additionally, multiple conditions may be chained in sequence:

```markdown
{{-*
{{#if updated}}
  This page was updated on {{updated}}.
{{#else if published}}
  This page was published on {{published}}.
{{#else}}
  This page is a draft.
{{#end}}
-}}
```
