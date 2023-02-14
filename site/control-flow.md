---
title: Control flow
layout: default
created: 2023-02-13T11:42:21-0800
---

## Conditionals

Conditionals in hakyllbars are expressed using the `if` helper function. Any expression may be used as a condition, and if it is _truthy_ the associated block will be shown.

```html
{{-*
{{#if published}}
  <p>Post published on {{published | dateAs shortDate}}.</p>
{{#end}}
-}}
```

Conditionals may also specify a default block:

```html
{{-*
{{#if published}}
  <p>Post published on {{published | dateAs shortDate}}.</p>
{{#else}}
  <p>Post is in draft.</p>
{{#end}}
-}}
```

Conditions may also be chained one after another:

```html
{{-*
{{#if updated}}
  <p>Post updated on {{updated | dateAs shortDate}}.</p>
{{#else if published}}
  <p>Post published on {{published | dateAs shortDate}}.</p>
{{#else}}
  <p>Post is in draft.</p>
{{#end}}
-}}
```

## Loops

Loops will use a list to display a block N times. If the list expression is empty or undefined, then no block is displayed. The context scope within the block is relative to the current item in the list.

```html
{{-*
<ul>
  {{#for post}}
    <li>{{title}} - {{published | dateAs shortDate}}</li>
  {{#end}}
</ul>
-}}
```

A default block may be used if the list is empty:

```html
{{-*
<ul>
  {{#for post}}
    <li>{{title}} - {{published | dateAs shortDate}}</li>
  {{#else}}
    <li>No posts published</li>
  {{#end}}
</ul>
-}}
```
