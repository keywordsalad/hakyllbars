module Hakyllbars.Template
  ( applyTemplates,
    applyContent,
    fileTemplate,
    applyCompiler,
    applyContext,
  )
where

import Control.Monad.State.Strict (evalStateT)
import Hakyll (defaultHakyllReaderOptions, defaultHakyllWriterOptions)
import Hakyllbars.Common
import Hakyllbars.Compiler
import Hakyllbars.Context
import Hakyllbars.Pandoc
import Text.Pandoc

applyTemplates :: TemplateRunner a () -> Item a -> Compiler (Item a)
applyTemplates templates item =
  evalStateT (templates >> tplItem) $
    TemplateState
      { tplContextStack = [],
        tplItemStack = [item],
        tplCallStack = ["item " ++ itemFilePath item]
      }

applyContent :: TemplateRunner String ()
applyContent = applyContentWith defaultHakyllReaderOptions defaultHakyllWriterOptions

applyContentWith :: ReaderOptions -> WriterOptions -> TemplateRunner String ()
applyContentWith readerOpts writerOpts = do
  applyAsTemplate
  tplModifyItem $ lift . pandocCompilerWith readerOpts writerOpts

fileTemplate :: FilePath -> TemplateRunner String ()
fileTemplate filePath =
  applyTemplate (fromFilePath filePath)

applyCompiler :: (Item a -> Compiler (Item a)) -> TemplateRunner a ()
applyCompiler compiler =
  tplModifyItem $ lift . compiler

applyContext :: Context a -> TemplateRunner a ()
applyContext = tplPushContext
{-# INLINE applyContext #-}
