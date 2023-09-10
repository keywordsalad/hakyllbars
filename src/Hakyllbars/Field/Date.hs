module Hakyllbars.Field.Date
  ( DateConfig (..),
    defaultDateConfigWith,
    dateFields,
    dateFormatField,
    dateField,
    publishedField,
    updatedField,
    getLastModifiedDate,
    isPublishedField,
    isUpdatedField,
    dateFromMetadata,
    normalizedDateTimeFormat,
    parseTimeM',
  )
where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (tails)
import Data.String.Utils
import Hakyllbars.Common
import Hakyllbars.Context
import Hakyllbars.Util

data DateConfig = DateConfig
  { dateConfigLocale :: TimeLocale,
    dateConfigCurrentTime :: ZonedTime,
    dateConfigDateLongFormat :: String,
    dateConfigDateShortFormat :: String,
    dateConfigTimeFormat :: String,
    dateConfigRobotDateFormat :: String,
    dateConfigRobotTimeFormat :: String
  }

defaultDateConfigWith :: TimeLocale -> ZonedTime -> DateConfig
defaultDateConfigWith locale currentTime =
  DateConfig
    { dateConfigLocale = locale,
      dateConfigCurrentTime = currentTime,
      dateConfigDateLongFormat = "%B %e, %Y %l:%M %P %EZ",
      dateConfigDateShortFormat = "%B %e, %Y",
      dateConfigTimeFormat = "%l:%M %p %EZ",
      dateConfigRobotDateFormat = "%Y-%m-%d",
      dateConfigRobotTimeFormat = "%Y-%m-%dT%H:%M:%S%Ez"
    }

dateFields :: DateConfig -> Context a
dateFields config =
  mconcat
    [ dateField "date" (dateConfigLocale config) (dateConfigCurrentTime config),
      publishedField "published" (dateConfigLocale config),
      updatedField "updated" (dateConfigLocale config),
      isPublishedField "isPublished",
      isUpdatedField "isUpdated",
      constField "longDate" (dateConfigDateLongFormat config),
      constField "shortDate" (dateConfigDateShortFormat config),
      constField "timeOnly" (dateConfigTimeFormat config),
      constField "robotDate" (dateConfigRobotDateFormat config),
      constField "robotTime" (dateConfigRobotTimeFormat config),
      constField "rfc822" rfc822DateFormat,
      dateFormatField "dateAs" (dateConfigLocale config)
    ]

dateFormatField :: String -> TimeLocale -> Context a
dateFormatField key timeLocale = functionField2 key f
  where
    f (dateFormat :: String) (dateString :: String) = do
      date <- deserializeTime dateString
      return $ formatTime timeLocale dateFormat date
    deserializeTime = parseTimeM' timeLocale normalizedDateTimeFormat

dateField :: String -> TimeLocale -> ZonedTime -> Context a
dateField key timeLocale currentTime = field key f
  where
    f item = do
      metadata <- lift . getMetadata $ itemIdentifier item
      tplWithCall key . lift $
        do
          let maybeDateString = dateFromMetadata timeLocale ["date", "published"] metadata
          maybe (dateFromFilePath timeLocale item) return maybeDateString
            <|> return (formatTime timeLocale "%Y-%m-%dT%H:%M:%S%Ez" currentTime)

publishedField :: String -> TimeLocale -> Context a
publishedField key timeLocale = field key f
  where
    f =
      lift
        . getMetadata
        . itemIdentifier
        >=> tplWithCall key
        . lift
        . maybe (noResult $ "Tried published field " ++ show key) return
        . dateFromMetadata timeLocale ["published", "date"]

updatedField :: String -> TimeLocale -> Context a
updatedField key timeLocale = field key f
  where
    f =
      lift
        . getMetadata
        . itemIdentifier
        >=> tplWithCall key
        . lift
        . maybe (noResult $ "Tried updated field " ++ show key) return
        . dateFromMetadata timeLocale ["updated", "published", "date"]

getLastModifiedDate :: TimeLocale -> Item a -> Compiler ZonedTime
getLastModifiedDate timeLocale item = do
  metadata <- getMetadata $ itemIdentifier item
  let maybeDateString = dateFromMetadata timeLocale ["updated", "published", "date"] metadata
  dateString <- maybe (dateFromFilePath timeLocale item) return maybeDateString
  parseTimeM' timeLocale "%Y-%m-%dT%H:%M:%S%Ez" dateString

dateFromMetadata :: TimeLocale -> [String] -> Metadata -> Maybe String
dateFromMetadata timeLocale sourceKeys metadata =
  firstAlt $ findDate <$> sourceKeys
  where
    findDate sourceKey =
      serializeTime =<< lookupString sourceKey metadata
    serializeTime dateString = do
      date <- firstAlt (parse dateString <$> metadataDateFormats)
      return $ normalizedTime timeLocale date
    parse = flip $ parseTimeM True timeLocale

dateFromFilePath :: TimeLocale -> Item a -> Compiler String
dateFromFilePath timeLocale item =
  dateFromPath
    <|> noResult ("Could not find file path date from " ++ show (toFilePath $ itemIdentifier item))
  where
    dateFromPath =
      firstAlt $
        dateFromPath' . intercalate "-"
          <$> ( [take 3 $ split "-" fnCand | fnCand <- reverse paths]
                  ++ (fmap (take 3) <$> reverse (tails paths))
              )
    paths = splitDirectories $ dropExtension $ toFilePath $ itemIdentifier item
    dateFromPath' path = do
      debugCompiler $ "Trying to parse date from path " ++ show path
      date <- parseTimeM' timeLocale "%Y-%m-%d" path
      return $ normalizedTime timeLocale date

parseTimeM' :: (MonadFail m) => TimeLocale -> String -> String -> m ZonedTime
parseTimeM' = parseTimeM True

normalizedTime :: TimeLocale -> ZonedTime -> String
normalizedTime = flip formatTime normalizedDateTimeFormat

normalizedDateTimeFormat :: String
normalizedDateTimeFormat = "%Y-%m-%dT%H:%M:%S%Ez"

rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %Z"

metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    normalizedDateTimeFormat,
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S %EZ",
    "%Y-%m-%d %H:%M:%S%Ez",
    "%Y-%m-%d %H:%M:%S",
    rfc822DateFormat,
    "%a, %d %b %Y %H:%M:%S",
    "%B %e, %Y %l:%M %p %EZ",
    "%B %e, %Y %l:%M %p",
    "%b %e, %Y %l:%M %p %EZ",
    "%b %e, %Y %l:%M %p",
    "%B %e, %Y",
    "%B %d, %Y",
    "%b %e, %Y",
    "%b %d, %Y"
  ]

isPublishedField :: String -> Context a
isPublishedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust
        . KeyMap.lookup (Key.fromString "published")

isUpdatedField :: String -> Context a
isUpdatedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust
        . KeyMap.lookup (Key.fromString "updated")
