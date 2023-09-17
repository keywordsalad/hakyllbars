---
title: Variables
layout: default
created: 2023-02-26T16:08:40-0800
---

Hakyllbars comes with a set of default variables. Variables are interpolated into templates by using `\{{` handlebars `\}}`.

## Basic variables

### `body`

The body of the current item being compiled.

### `host`

The site hostname. This variable is configurable.

Here, the value of `{{*{{host}}}}` is `{{host}}`.

### `siteRoot`

The root of the site, consisting of the hostname and any path. This variable is configurable.

Here, the value of `{{*{{siteRoot}}}}` in `{{siteRoot}}`.

### `siteUrl`

The absolute url to the site root. A helper that concatenates `{{*{{host}}{{siteRoot}}}}`.

Here, the value of `{{*{{siteUrl}}}}` is `{{siteUrl}}`.

### `path`

The path to the current item being compiled.

Here, the `{{*{{path}}}}` is `{{path}}`.

### `url`

The URL to the current item being compiled.

Here, the `{{*{{url}}}}` is `{{url}}`.

### `absUrl`

The absolute URL to the current item being compiled including hostname.

Here, the `{{*{{absUrl}}}}` is `{{absUrl}}`.

### `updated`

Gets when the current item being compiled was _updated_, _published_, or _created_ via metadata.

Here, the `{{*{{updated}}}}` is `{{updated}}`.

### `published`

Gets when the current item being compiled with _published_ or _created_ via metadata.

Here, the `{{*{{published}}}}` is `{{published}}`.

### `title`

Gets the title of the current item being compiled from metadata or derives the title from the item's filename.

Here, the `{{*{{title}}}}` is `{{title}}`.

## git variables

These variables are useful for displaying metadata on pages about when they were last modified.

### `gitWebUrl`

The URL to the site's git repository web viewer. This variable is configurable.

Here, the `{{*{{gitWebUrl}}}}` is `{{gitWebUrl}}`.

### `gitSha1`

The git SHA-1 hash of the current item being compiled.

Here, the `{{*{{gitSha1}}}}` is `{{gitSha1}}`.

### `gitMessage`

The latest git commit message for which the current item was last modified.

Here, the `{{*{{gitMessage}}}}` is `{{gitMessage}}`.

### `gitBranch`

The git branch from which the current item is compiled.

Here, the `{{*{{gitBranch}}}}` is `{{gitBranch}}`.

### `gitFilePath`

The path to the current item being compiled within the git repository.

Here, the `{{*{{gitFilePath}}}}` is `{{gitFilePath}}`.

### `gitFileName`

The filename of the current file being compiled within the git repository.

Here, the `{{*{{gitFileName}}}}` is `{{gitFileName}}`.

### `isFromSource`

Indicates whether the current item being compiled is produced from source or is instead produced from generated data.

Here, the `{{*{{isFromSource}}}}` is `{{isFromSource}}`.

### `isChanged`

Indicates whether the current item being compiled has uncommitted changes.

Here, the `{{*{{isChanged}}}}` is `{{isChanged}}`.
