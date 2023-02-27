module Main where

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String.Utils (join, split)
import Data.Time
import Hakyll
import Hakyllbars as HB
import System.Environment (lookupEnv)
import Text.Pandoc.Highlighting (haddock, styleToCss)

main :: IO ()
main = do
  deployEnv <- fromMaybe Prod . ((=<<) deployEnvFromStr) <$> lookupEnv "DEPLOY_ENV"
  time <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
  let dateConfig = HB.defaultDateConfigWith defaultTimeLocale time
  hakyllWith config do
    css' <- css
    js' <- js
    templates' <- templates
    rulesExtraDependencies [css', js', templates'] do
      pages deployEnv dateConfig

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
  create ["css/code-style.css"] do
    route idRoute
    compile $ makeItem (styleToCss haddock)
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
      -- Get the content of the template
      getResourceBody
        -- Compile the template
        >>= HB.compileTemplateItem
        -- Create an item from the template
        >>= makeItem
  makePatternDependency templatePattern
  where
    templatePattern =
      "_layouts/**"
        .||. "_partials/**"
        .||. "_templates/**"
        .||. "_sections/**"

pages :: DeployEnv -> HB.DateConfig -> Rules ()
pages deployEnv dateConfig = do
  match "*.md" do
    route $ setExtension "html" `composeRoutes` indexRoute
    -- This is where Hakyllbars is applied
    compile do
      getResourceBody >>= HB.applyTemplates do
        -- Sets the root context used in the templates
        HB.applyContext (context deployEnv dateConfig)
        -- Applies the item content as a template
        HB.applyContent
        -- Applies the final template
        HB.applyTemplate "_layouts/from-context.html"

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
context :: DeployEnv -> HB.DateConfig -> HB.Context String
context deployEnv dateConfig =
  HB.gitFields "site" "https://github.com/keywordsalad/hakyllbars/tree"
    <> HB.includeField "section" (Just "_sections") (Just ".md")
    <> HB.includeField "partial" (Just "_partials") (Just ".md")
    <> HB.dateFields dateConfig
    -- Using the default fields is very recommended
    <> HB.defaultFields host siteRoot
  where
    host = deployEnvHost deployEnv
    siteRoot = deployEnvSiteRoot deployEnv

data DeployEnv = Prod | Dev

deployEnvFromStr :: String -> Maybe DeployEnv
deployEnvFromStr = \case
  "prod" -> Just Prod
  "dev" -> Just Dev
  _ -> Nothing

deployEnvHost :: DeployEnv -> String
deployEnvHost = \case
  Prod -> "https://keywordsalad.github.io"
  Dev -> "http://localhost:8000"

deployEnvSiteRoot :: DeployEnv -> String
deployEnvSiteRoot = \case
  Prod -> "/hakyllbars"
  Dev -> ""
