module Main where

import Data.List (isSuffixOf)
import Data.String.Utils (join, split)
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
      route $ setExtension "html" `composeRoutes` indexRoute
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

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml
  where
    appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | ".html" `isSuffixOf` x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a
