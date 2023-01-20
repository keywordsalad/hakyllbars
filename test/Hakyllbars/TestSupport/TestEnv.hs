module Hakyllbars.TestSupport.TestEnv where

import Data.Foldable
import Data.Time
import Hakyll (Configuration (..), removeDirectory)
import qualified Hakyll.Core.Provider as HP
import qualified Hakyll.Core.Store as HS
import Hakyllbars.Common
import Hakyllbars.Field.Date
import Hakyllbars.TestSupport.Config

data TestEnv = TestEnv
  { testTime :: ZonedTime,
    testHakyllConfig :: Configuration,
    testStore :: HS.Store,
    testProvider :: HP.Provider,
    testDateConfig :: DateConfig
  }

newStoreAndProvider :: Configuration -> IO (HS.Store, HP.Provider)
newStoreAndProvider hakyllConfig = do
  store <- newStore
  provider <- newProvider store
  return (store, provider)
  where
    newStore = HS.new True (storeDirectory hakyllConfig)
    newProvider store = HP.newProvider store (const (return False)) (providerDirectory hakyllConfig)

createTestEnv :: Configuration -> IO TestEnv
createTestEnv hakyllConfig = do
  time <- defaultTestTime
  (store, provider) <- newStoreAndProvider hakyllConfig
  return
    TestEnv
      { testTime = time,
        testHakyllConfig = hakyllConfig,
        testStore = store,
        testProvider = provider,
        testDateConfig = defaultDateConfigWith defaultTimeLocale time
      }

cleanTestEnv :: TestEnv -> IO ()
cleanTestEnv testEnv =
  let hakyllConfig = testHakyllConfig testEnv
   in traverse_ (removeDirectory . ($ hakyllConfig)) cleanDirectories
  where
    cleanDirectories =
      [ destinationDirectory,
        storeDirectory,
        tmpDirectory
      ]

withTestEnv :: Configuration -> (TestEnv -> IO a) -> IO a
withTestEnv hakyllConfig = bracket (createTestEnv hakyllConfig) cleanTestEnv

withDefaultTestEnv :: (TestEnv -> IO a) -> IO a
withDefaultTestEnv = withTestEnv defaultHakyllConfig

defaultTestTimeString :: String
defaultTestTimeString = "2023-06-16T21:12:00-07:00"

defaultTestTime :: (MonadFail m) => m ZonedTime
defaultTestTime = timeFromString defaultTestTimeString

timeFromString :: (MonadFail m) => String -> m ZonedTime
timeFromString = parseTimeM True defaultTimeLocale "%FT%T%EZ"
