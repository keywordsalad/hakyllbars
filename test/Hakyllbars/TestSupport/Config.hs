module Hakyllbars.TestSupport.Config where

import Hakyll (Configuration (..), defaultConfiguration)

defaultHakyllConfig :: Configuration
defaultHakyllConfig =
  defaultConfiguration
    { destinationDirectory = "_test/site",
      storeDirectory = "_test/store",
      tmpDirectory = "_test/tmp",
      providerDirectory = "test/data"
    }
