#!/usr/bin/env bash
# This "./go" script is the build script.
# For context behind the "./go" script, please read these:
# https://blog.thepete.net/blog/2014/03/28/_-attributes-of-an-amazing-dev-toolchain/
# https://code.ofvlad.xyz/vlad/lightning-runner
set -e

_verify-prerequisites () {
  git config core.hooksPath .githooks

  if ! command -v stack &> /dev/null
  then
      _bad-message "Install haskell-stack to continue"
      exit 1
  fi
}

⚡build () {
  _help-line "Compile Hakyllbars and generate the site"
  stack build
}

⚡build_site () {
  _help-line "Generate the site"
  ⚡build
  stack exec site build
}

⚡clean () {
  _help-line "Clean everything"
  ⚡clean_site
  stack clean
}

⚡clean_site () {
  _help-line "Clean the generated site"
  rm -rf _cache
}

⚡rebuild () {
  _help-line "Clean and rebuild everything"
  ⚡clean
  ⚡build "$@"
}

⚡rebuild_site () {
  _help-line "Clean and regenerate the site"
  ⚡clean_site
  ⚡build_site
}

⚡watch_site () {
  _help-line "Preview the site"
  ⚡rebuild_site
  stack exec site watch
}

⚡test () {
  _help-line "Run hspec tests"
  stack test
}

source ⚡
