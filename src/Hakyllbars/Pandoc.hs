module Hakyllbars.Pandoc where

import Hakyll hiding (pandocCompilerWith)
import System.FilePath
import Text.Pandoc

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions

pandocCompilerWith :: ReaderOptions -> WriterOptions -> Item String -> Compiler (Item String)
pandocCompilerWith readerOpts writerOpts item@(Item id' _) = do
  let ext = takeExtension $ toFilePath id'
  go ext
  where
    go ".html" = return item
    go _ = renderPandocWith readerOpts writerOpts item
