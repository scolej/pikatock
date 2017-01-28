module Cli.Opt
  ( Opt(..)
  , parseArgs
  ) where

import Control.Monad
import Data.Time.Calendar
import Pikatock.Parser
import System.Console.GetFlag

parseArgs :: [String] -> Either String ([Opt], [String])
parseArgs argStrs = do
  let (opts, args, errs) = getOpt Permute options argStrs
  unless (null errs) (fail $ "\n" ++ concat errs)
  when (null args) (fail usage)
  case sequence opts >>= validateOpts of
    Left e -> fail e
    Right o -> Right (o, args)

validateOpts :: [Opt] -> Either String [Opt]
validateOpts opts = Right opts -- TODO

usage :: String
usage = usageInfo "pikatock" options

-- | Command line options.
data Opt
  = OptBefore Day
  | OptAfter Day
  | OptToday
  | OptWorkWeek
  | OptWeekly
  | OptDaily
  | OptHelp
  deriving (Show)

-- | Command line option descriptions.
options :: [OptDescr (Either String Opt)]
options =
  [ Option
      "b"
      (ReqArg (\s -> OptBefore <$> readDate s) "yyyy-mm-dd")
      "Only include entries before this date."
  , Option
      "a"
      (ReqArg (\s -> OptAfter <$> readDate s) "yyyy-mm-dd")
      "Only include entries after this date."
  , Option "t" (NoArg (Right OptToday)) "Only include entries for today."
  , Option
      "w"
      (NoArg (Right OptWorkWeek))
      "Only include entries for this work week (Monday-Friday)."
  , Option "dd" (NoArg (Right OptDaily)) "Show one set of results for each day."
  -- TODO , Option
  --     "ww"
  --     (NoArg (Right OptWeekly))
  --     "Show one set of results for each week."
  -- TODO , Option "h" (NoArg (Right OptHelp)) "Show this help."
  ]
