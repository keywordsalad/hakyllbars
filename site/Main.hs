module Main where

import Data.List (isSuffixOf)
import Data.String.Utils (join, split)
import Hakyll hiding (Context, Template, applyTemplate, compileTemplateItem, constField)
import Hakyllbars
import Text.Pandoc
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

context :: Context String
context =
  constField "codeStyle" (styleToCss haddock)
    <> gitFields "site" "https://github.com/keywordsalad/hakyllbars"
    <> layoutField "applyLayout" "_layouts"
    <> defaultFields

applyLayout :: TemplateRunner String ()
applyLayout = applyTemplate "_layouts/from-context.html"

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

pages :: Rules ()
pages = do
  match "*.md" do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile do
      getResourceBody >>= applyTemplates do
        applyContext context
        applyContentWith readerOptions writerOptions
        applyLayout

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml
  where
    appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | ".html" `isSuffixOf` x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        foldl
          (flip ($))
          (readerExtensions defaultHakyllReaderOptions)
          [ enableExtension Ext_smart,
            enableExtension Ext_inline_code_attributes,
            disableExtension Ext_markdown_in_html_blocks
          ]
    }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions {writerHighlightStyle = Just haddock}
