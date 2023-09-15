module Hakyllbars.Field.Html
  ( escapeHtmlField,
    escapeHtmlUriField,
  )
where

import Hakyllbars.Common
import Hakyllbars.Context
import Network.URI (escapeURIString, isUnescapedInURI)

-- | Escapes HTML before interpolating in a template.
escapeHtmlField :: Context String
escapeHtmlField = functionField "escapeHtml" f
  where
    f = return . escapeHtml

-- | Escapes HTML URI before interpolating in a template.
escapeHtmlUriField :: Context String
escapeHtmlUriField = functionField "escapeHtmlUri" f
  where
    f = return . escapeHtml . escapeURIString isUnescapedInURI
