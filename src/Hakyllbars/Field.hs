{-# LANGUAGE AllowAmbiguousTypes #-}

module Hakyllbars.Field
  ( module Hakyllbars.Field.Date,
    module Hakyllbars.Field.Git,
    module Hakyllbars.Field.Html,
    defaultFields,
    emptyString,
    defaultKeys,
    includeField,
    layoutField,
    ifField,
    forField,
    withField,
    forEachField,
    defaultField,
    linkedTitleField,
    metadataField,
    siteUrlField,
    urlField,
    absUrlField,
    getUrlField,
    getAbsUrlField,
    titleFromFileField,
    teaserField,
    metadataPriorityField,
    namedMetadataField,
    putField,
    addField,
    putBlockField,
    addBlockField,
  )
where

import Control.Monad.Except
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Hakyllbars.Ast
import Hakyllbars.Common
import Hakyllbars.Compiler
import Hakyllbars.Context
import Hakyllbars.Field.Date (DateConfig, dateFields, defaultDateConfigWith)
import Hakyllbars.Field.Git (gitFields)
import Hakyllbars.Field.Html (escapeHtmlField, escapeHtmlUriField)
import Hakyllbars.Util (stripSuffix)
import System.FilePath

defaultFields :: String -> String -> Context String
defaultFields host siteRoot =
  mconcat
    [ bodyField "body",
      constField "host" host,
      constField "siteRoot" siteRoot,
      pathField "path",
      siteUrlField "siteUrl" "host" "siteRoot",
      urlField "url" "siteRoot",
      absUrlField "absUrl" "host" "url",
      getUrlField "getUrl" "siteRoot",
      getAbsUrlField "getAbsUrl" "host" "getUrl",
      linkedTitleField "linkedTitle" "title" "url",
      escapeHtmlField,
      escapeHtmlUriField,
      putField "put",
      addField "add",
      putBlockField "putBlock",
      addBlockField "addBlock",
      ifField "if",
      forField "for",
      defaultField "default",
      withField "with",
      includeField "include" Nothing Nothing,
      includeField "partial" (Just "_partials") (Just "html"),
      layoutField "applyLayout" "_layouts" (Just "html"),
      metadataPriorityField "updated" ["updated", "published", "created"],
      metadataPriorityField "published" ["published", "created"],
      metadataField,
      titleFromFileField "title",
      constField "description" ("" :: String)
    ]

emptyString :: ContextValue a
emptyString = intoValue ("" :: String)

defaultKeys :: [String] -> Context a
defaultKeys keys = intoContext $ (,"" :: String) <$> keys

withField :: String -> Context String
withField key = functionField2 key f
  where
    f (context :: Context String) (blocks :: [Block]) =
      tplWithContext context do
        reduceBlocks blocks

includeField :: String -> Maybe FilePath -> Maybe FilePath -> Context String
includeField key basePath extension = functionField key f
  where
    f (filePath :: String) = do
      basePath' <- maybe (itemFilePath <$> tplItem) return basePath
      let filePath' = basePath' </> filePath
      let filePath'' = maybe filePath' (filePath' <.>) extension
      context <- tplContext
      applyTemplate (fromFilePath filePath'')
      itemValue context <$> tplPopItem

layoutField :: String -> FilePath -> Maybe FilePath -> Context String
layoutField key basePath extension = functionField2 key f
  where
    f (filePath :: FilePath) (content :: String) = do
      let filePath' = basePath </> filePath
      let filePath'' = maybe filePath' (filePath' <.>) extension
      let layoutId = fromFilePath filePath''
      (Template bs _) <- loadTemplate layoutId
      item <- itemSetBody content <$> tplItem
      tplWithItem item do
        reduceBlocks bs

ifField :: forall a. String -> Context a
ifField key = functionField key isTruthy

forField :: String -> Context String
forField key = functionField2 key applyForLoop

applyForLoop :: ContextValue String -> [Block] -> TemplateRunner String (Maybe String)
applyForLoop items blocks =
  getAsItems items
    `catchError` (\_ -> getAsStrings items)
    `catchError` (\_ -> return (mempty, []))
    >>= uncurry go
  where
    go context items'
      | null items' = return Nothing
      | otherwise = tplWithContext context do
          Just . mconcat <$> forM items' \item ->
            tplWithItem item do
              reduceBlocks blocks

getAsItems :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsItems = fromValue

getAsStrings :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsStrings x = do
  bodies <- fromValue x :: TemplateRunner String [String]
  items <- forM bodies \body -> itemSetBody body <$> tplItem
  return (bodyField "item", items)

forEachField :: String -> Context String
forEachField key = functionField3 key f
  where
    f (forEachKey :: ContextValue String) (forEachItems :: ContextValue String) (blocks :: [Block]) = do
      keyId <- getKey forEachKey
      keyItemPairs <- fromValue forEachItems :: TemplateRunner String [(ContextValue String, ContextValue String)]
      keyItemPairs `forM` \(key', items) ->
        tplWithContext (constField keyId key') do
          applyForLoop items blocks
    getKey block = case block of
      UndefinedValue k _ _ _ -> return k -- allow identifier as key
      StringValue k -> return k
      _ -> tplFail "forEach: key must be a string or identifier"

defaultField :: forall a. String -> Context a
defaultField key = functionField2 key f
  where
    f (default' :: ContextValue a) (arg :: ContextValue a) =
      isTruthy arg <&> \case
        True -> arg
        False -> default'

linkedTitleField :: String -> String -> String -> Context String
linkedTitleField key titleKey urlKey = constField key f
  where
    f :: FunctionValue String String String
    f filePath = do
      tplWithItem (Item (fromFilePath filePath) "") do
        makeLink <$> getField titleKey <*> getField urlKey
      where
        getField key' = do
          context <- tplContext
          fromValue =<< unContext context key'
        makeLink title url
          | ".html" `isSuffixOf` filePath = "<a href=\"" ++ escapeHtml url ++ "\" title=\"" ++ escapeHtml title ++ "\">" ++ escapeHtml title ++ "</a>"
          | ".md" `isSuffixOf` filePath = "[" ++ title ++ "](" ++ url ++ " \"" ++ title ++ "\")"
          | otherwise = title ++ " <" ++ url ++ ">"

metadataField :: forall a. Context a
metadataField = Context f
  where
    f key = lift . getMetadataField key =<< tplItem

getMetadataField :: String -> Item a -> Compiler (ContextValue a)
getMetadataField key item = do
  m <- getMetadata (itemIdentifier item)
  maybe
    (noResult $ "tried metadata key " ++ show key)
    (return . intoValue)
    (KeyMap.lookup (Key.fromString key) m)

bodyField :: String -> Context String
bodyField key = field key $ return . itemBody

siteUrlField :: String -> String -> String -> Context a
siteUrlField key hostKey siteRootKey = field key f
  where
    f _ = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      siteRoot <- fromValue =<< unContext context siteRootKey
      return (host ++ siteRoot :: String)

urlField :: String -> String -> Context a
urlField key siteRootKey = field key f
  where
    f = getUri key siteRootKey . itemIdentifier

getUrlField :: String -> String -> Context a
getUrlField key siteRootKey = functionField key f
  where
    f = getUri key siteRootKey . fromFilePath

getUri :: String -> String -> Identifier -> TemplateRunner a String
getUri key siteRootKey id' = do
  siteRoot <-
    tplContext
      >>= flip unContext siteRootKey
      >>= fromValue
  maybeRoute <- lift $ getRoute id'
  definitelyRoute <-
    maybe
      (fail $ "no route by " ++ show key ++ " found for item " ++ show id')
      (return . ("/" ++))
      maybeRoute
  let uri = stripSuffix "index.html" definitelyRoute
  return if null uri then siteRoot else siteRoot ++ uri

absUrlField :: String -> String -> String -> Context a
absUrlField key hostKey urlKey = field key f
  where
    f _ = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      url <- fromValue =<< unContext context urlKey
      return (host ++ url :: String)

getAbsUrlField :: forall a. String -> String -> String -> Context a
getAbsUrlField key hostKey getUrlKey = functionField key f
  where
    f (filePath :: FilePath) = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      getUrl <- fromValue =<< unContext context getUrlKey
      url <- getUrl (intoValue filePath :: ContextValue a)
      return (host ++ url :: String)

pathField :: String -> Context a
pathField key = field key $ return . toFilePath . itemIdentifier

titleFromFileField :: String -> Context a
titleFromFileField = bindField titleFromPath . pathField
  where
    titleFromPath = return . takeBaseName

teaserField :: String -> Snapshot -> Context String
teaserField key snapshot = field key f
  where
    f item = lift do
      body <- loadSnapshotBody (itemIdentifier item) snapshot
      case takeTeaser body of
        Just teaser -> return teaser
        Nothing -> fail $ "item " ++ itemFilePath item ++ " has no teaser"
    takeTeaser = go ""
      where
        go acc xss@(x : xs)
          | "<!--more-->" `isPrefixOf` xss = Just (reverse acc)
          | otherwise = go (x : acc) xs
        go _ [] = Nothing

metadataPriorityField :: String -> [String] -> Context a
metadataPriorityField key priorityKeys = field key f
  where
    f item =
      lift $
        foldl
          (<|>)
          (noResult $ "Metadata priority key " ++ show key ++ " from set " ++ show priorityKeys)
          (flip getMetadataField item <$> priorityKeys)

namedMetadataField :: String -> Context String
namedMetadataField key = field key $ lift . getMetadataField key

putField :: String -> Context a
putField key = functionField key tplPut

addField :: forall a. String -> Context a
addField key = functionField2 key f
  where
    f (name :: String) (value :: ContextValue a) = do
      current <- tplGet name `catchError` \_ -> return []
      tplPut $ constField name (value : current)

putBlockField :: String -> Context a
putBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      tplPut $ constField name blocks

addBlockField :: String -> Context a
addBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      current <- tplGet name `catchError` \_ -> return []
      tplPut $ constField name (current ++ blocks)
