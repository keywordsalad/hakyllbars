module Main where

import Data.List (isSuffixOf)
import Data.String.Utils (join, split)
import Hakyll hiding (Context, Template, applyTemplate, compileTemplateItem, constField)
import Hakyllbars
import Text.Pandoc.Highlighting (haddock, styleToCss)

main :: IO ()
main = hakyllWith config do
  css' <- css
  js' <- js
  templates' <- templates
  rulesExtraDependencies [css', js', templates'] do
    pages

config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "site",
      destinationDirectory = "_site"
    }

css :: Rules Dependency
css = do
  match "css/**" do
    route idRoute
    compile getResourceBody
  makePatternDependency "css/**"

js :: Rules Dependency
js = do
  match "js/**" do
    route idRoute
    compile getResourceBody
  makePatternDependency "js/**"

-- Precompile all templates
templates :: Rules Dependency
templates = do
  match templatePattern do
    -- Use this to compile templates
    compile $
      getResourceBody -- Get the content of the template
        >>= compileTemplateItem -- Compile the template
        >>= makeItem -- Create an item from the template
  makePatternDependency templatePattern
  where
    templatePattern =
      "_layouts/**"
        .||. "_partials/**"
        .||. "_templates/**"

pages :: Rules ()
pages = do
  match "*.md" do
    route $ setExtension "html" `composeRoutes` indexRoute
    -- This is where Hakyllbars is applied
    compile do
      getResourceBody >>= applyTemplates do
        applyContext context -- Sets the root context within the templates
        applyContent -- Applies the item content as a template
        applyTemplate "_layouts/from-context.html" -- Applies the final template

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml
  where
    appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | ".html" `isSuffixOf` x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a

-- This uses Hakyllbars' Context, not Hakyll's
context :: Context String
context =
  siteRootField "/hakyllbars"
    <> constField "codeStyle" (styleToCss haddock)
    <> gitFields "site" "https://github.com/keywordsalad/hakyllbars/tree"
    <> layoutField "applyLayout" "_layouts" -- Configure layouts to load from _layouts/
    <> defaultFields -- Using the default fields is very recommended
