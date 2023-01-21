module Main where

import Hakyll hiding (Context, Template, applyTemplate, compileTemplateItem)
import Hakyllbars

main :: IO ()
main = hakyllWith config do
  match "css/**" do
    route idRoute
    compile getResourceBody
  --
  cssDependencies <- makePatternDependency "css/**"
  templatesDependency <- templates
  rulesExtraDependencies [cssDependencies, templatesDependency] do
    --
    match "*.md" do
      route $ setExtension "html"
      compile do
        getResourceBody >>= applyTemplates do
          applyContext context
          applyContent
          applyLayout

config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "site",
      destinationDirectory = "_site"
    }

context :: Context String
context =
  gitFields "site" "https://github.com/keywordsalad/hakyllbars"
    <> layoutField "applyLayout" "_layouts"
    <> defaultFields

applyLayout :: TemplateRunner String ()
applyLayout = applyTemplate "_layouts/from-context.html"

templates :: Rules Dependency
templates = do
  match templatePattern do
    compile $
      getResourceBody
        >>= compileTemplateItem
        >>= makeItem
  makePatternDependency templatePattern
  where
    templatePattern =
      "_layouts/**"
        .||. "_partials/**"
        .||. "_templates/**"
