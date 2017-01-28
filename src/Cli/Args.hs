module Cli.Args
  ( PikaArgs(..)
  , readArgs
  ) where

import Cli.Opt
import Control.Arrow
import qualified Data.Map.Strict as M
import Data.Time.Calendar
import Pikatock.Entry
import Pikatock.Util

data PikaArgs = PikaArgs
  { argsFiles :: [String]
  , argsFilter :: Entry -> Bool
  , argsGrouper :: [Entry] -> [(String, [Entry])]
  }

defaultArgs :: PikaArgs
defaultArgs = PikaArgs [] (const True) (\es -> [("All entries.", es)])

readArgs :: Day -> [String] -> Either String PikaArgs
readArgs today as = interpretArgs today <$> parseArgs as

interpretArgs :: Day -> ([Opt], [String]) -> PikaArgs
interpretArgs today (opts, args) =
  foldl (argMut today) (defaultArgs {argsFiles = args}) opts

argMut :: Day -> PikaArgs -> Opt -> PikaArgs
argMut today pa opt = pa'
  where
    f0 = argsFilter pa
    pa' =
      case opt of
        OptToday -> pa {argsFilter = \e -> entryDay e == today}
        OptWorkWeek -> pa {argsFilter = entryInWorkWeek today}
        OptBefore d -> pa {argsFilter = \e -> f0 e && entryDay e <= d}
        OptAfter d -> pa {argsFilter = \e -> f0 e && entryDay e >= d}
        OptWeekly -> pa
        OptDaily -> pa {argsGrouper = dailyGrouper}
        _ -> pa

dailyGrouper :: [Entry] -> [(String, [Entry])]
dailyGrouper allEs =
  let kvs = map (\e -> (entryDay e, [e])) allEs -- List of keys and values
      kvs' = (M.toList . M.fromListWith (++)) kvs
      svs = map (first show) kvs'
  in svs
