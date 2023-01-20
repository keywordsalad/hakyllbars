module Hakyllbars.Field.Git
  ( gitFields,
    gitSha1Compiler,
    gitMessageCompiler,
    gitLogField,
    gitFileField,
    gitFileCompiler,
    gitBranchCompiler,
    gitBranch,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Hakyllbars.Common
import Hakyllbars.Context
import System.Exit
import System.Process

gitFields :: String -> String -> Context a
gitFields providerDirectory gitWebUrl =
  mconcat
    [ constField "gitWebUrl" gitWebUrl,
      field "gitSha1" (gitSha1Compiler providerDirectory),
      field "gitMessage" (gitMessageCompiler providerDirectory),
      field "gitBranch" gitBranchCompiler,
      gitFileField providerDirectory "gitFilePath" gitFilePath,
      gitFileField providerDirectory "gitFileName" (takeFileName . gitFilePath),
      gitFileField providerDirectory "isFromSource" gitFileIsFromSource,
      gitFileField providerDirectory "isChanged" gitFileIsChanged
    ]

gitSha1Compiler :: String -> Item a -> TemplateRunner a String
gitSha1Compiler = gitLogField "%h"

gitMessageCompiler :: String -> Item a -> TemplateRunner a String
gitMessageCompiler = gitLogField "%s"

type LogFormat = String

gitLogField :: LogFormat -> String -> Item a -> TemplateRunner a String
gitLogField format providerDirectory item =
  lift $ unsafeCompiler do
    maybeResult <- gitLog format (Just $ providerDirectory </> toFilePath (itemIdentifier item))
    case maybeResult of
      Just result -> return result
      Nothing -> fromJust <$> gitLog format Nothing

data GitFile = GitFile
  { gitFilePath :: String,
    gitFileIsFromSource :: Bool,
    gitFileIsChanged :: Bool
  }
  deriving (Generic)

instance Binary GitFile where
  get = GitFile <$> get <*> get <*> get
  put (GitFile x y z) = put x >> put y >> put z

gitFileField :: (IntoValue v a) => String -> String -> (GitFile -> v) -> Context a
gitFileField providerDirectory key f = field key $ fmap f . gitFileCompiler providerDirectory

gitFileCompiler :: String -> Item a -> TemplateRunner a GitFile
gitFileCompiler providerDirectory item =
  lift $
    GitFile gitFilePath
      <$> unsafeCompiler (doesFileExist gitFilePath)
      <*> unsafeCompiler (isChanged gitFilePath)
  where
    gitFilePath = providerDirectory </> toFilePath (itemIdentifier item)
    isChanged filePath = do
      let args = ["diff", "HEAD", filePath]
      (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
      return $ not (exitCode == ExitSuccess && null stdout)

gitLog :: LogFormat -> Maybe FilePath -> IO (Maybe String)
gitLog format filePath = do
  let fpArgs = bool [] [fromJust filePath] (isJust filePath)
  let args = ["log", "-1", "HEAD", "--pretty=format:" ++ format] ++ fpArgs
  (_exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
  return if null stdout then Nothing else Just stdout

gitBranchCompiler :: Item a -> TemplateRunner a String
gitBranchCompiler _ = lift $ unsafeCompiler gitBranch

gitBranch :: IO String
gitBranch = do
  let args = ["branch", "--show-current"]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return stdout
    else fail $ "Unable to get current branch: " ++ stderr
