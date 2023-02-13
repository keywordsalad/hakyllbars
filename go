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

  if ! command -v hakyll-init &> /dev/null
  then
    stack install hakyll
    if [ $? -ne 0 ]; then
      _bad-message "Failed to install Hakyll, check README.md for troubleshooting"
      exit 1
    fi
  fi
}

⚡build () {
  _help-line "Compile hakyllbars and generate the site"
  stack build
  stack exec site build -- "$@"
}

⚡clean () {
  _help-line "Clean generated site files"
  rm -rf _cache/* _site/*
}

⚡clean_all () {
  _help-line "Clean generated site files and site generator binaries"
  ⚡clean
  stack clean
}

⚡rebuild () {
  _help-line "Clean and then rebuild the generated site"
  ⚡clean
  ⚡build "$@"
}

⚡rebuild_all () {
  _help-line "Clean and then rebuild both the generated site and the site generator binary"
  ⚡clean_all
  ⚡build "$@"
}

⚡prebake() {
  _help-line "Compile only the site generator's and tests' dependencies"
  stack build --only-dependencies
  stack test --only-dependencies
}

⚡watch () {
  _help-line "Build the site generator, generate the site, and then run the preview server"
  ⚡build
  stack exec site watch -- "$@"
}

⚡rewatch() {
  _help-line "Rebuild the site generator, regenerate the site, and then run the preview server"
  ⚡rebuild
  stack exec site watch -- "$@"
}

⚡kill() {
  _help-line "Kill the site preview server if has gotten loose and run away!"
  lsof -ti tcp:8000 | xargs kill -9
}

⚡publish () {
  _help-line "Build the site and then publish it live"
  current_branch="$(git branch --show-current)"
  if [[ "$current_branch" != "main" ]]; then
    _bad-message "Can only publish from main branch; tried to publish from $current_branch"
    exit 1
  fi
  ⚡test_sync "main"

  sha="$(git log -1 HEAD --pretty=format:%h)"

  git fetch origin _site
  mkdir -p _site
  rm -rf _site/* _site/.git
  cp -r .git/ ./_site/.git/
  pushd ./_site
  git switch _site
  git pull origin _site
  popd

  DEPLOY_ENV=PROD ⚡rebuild

  pushd ./_site
  git add .
  git commit -m "Build on $(date) generated from $sha"
  git push origin _site
  popd
}

⚡test () {
  _help-line "Run hspec tests"
  stack test
}

source ⚡
