---
title: Variables
layout: default
created: 2023-02-26T16:08:40-0800
---

Hakyllbars comes with a set of default variables.

## Basic variables

### `body`

The body of the current item being compiled.

### `host`

The site hostname. This variable is configurable.

### `siteRoot`

The root of the site, consisting of the hostname and any path. This variable is configurable.

### `path`

The path to the current item being compiled.

### `url`

The URL to the current item being compiled.

### `absUrl`

The absolute URL to the current item being compiled including hostname.

### `updated`

Gets when the current item being compiled was _updated_, _published_, or _created_ via metadata.

### `published`

Gets when the current item being compiled with _published_ or _created_ via metadata.

### `title`

Gets the title of the current item being compiled from metadata or derives the title from the item's filename.

## git variables

These variables are useful for displaying metadata on pages about when they were last modified.

### `gitWebUrl`

The URL to the site's git repository web viewer. This variable is configurable.

### `gitSha1`

The git SHA-1 hash of the current item being compiled.

### `gitMessage`

The latest git commit message for which the current item was last modified.

### `gitBranch`

The git branch from which the current item is compiled.

### `gitFilePath`

The path to the current item being compiled within the git repository.

### `gitFileName`

The filename of the current file being compiled within the git repository.

### `isFromSource`

Indicates whether the current item being compiled is produced from source or is instead produced from generated data.

### `isChanged`

Indicates whether the current item being compiled has uncommitted changes.
