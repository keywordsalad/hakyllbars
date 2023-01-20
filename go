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
  _help-line "Compile the site generator and generate the site"
  stack build
}

⚡clean () {
  _help-line "Clean compiled output"
  stack clean
}

⚡rebuild () {
  _help-line "Clean and then rebuild"
  ⚡clean
  ⚡build "$@"
}

⚡test () {
  _help-line "Run hspec tests"
  stack test
}

source ⚡
