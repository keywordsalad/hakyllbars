module Hakyllbars.Field.Html
  ( escapeHtmlField,
    escapeHtmlUriField,
  )
where

import Hakyllbars.Common
import Hakyllbars.Context
import Network.URI (escapeURIString, isUnescapedInURI)

escapeHtmlField :: Context String
escapeHtmlField = functionField "escapeHtml" f
  where
    f = return . escapeHtml

escapeHtmlUriField :: Context String
escapeHtmlUriField = functionField "escapeHtmlUri" f
  where
    f = return . escapeHtml . escapeURIString isUnescapedInURI
