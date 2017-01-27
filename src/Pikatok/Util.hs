module Pikatok.Util where

import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Numeric
import Pikatok.Entry
import Pikatok.Tree

-- | Map a user's entry to the list of tags which categorize it and a
-- value which can be mappended with all the other user entries. This
-- is used when collecting the entries into a tree.
calcDuration :: Entry -> ([String], Sum Float)
calcDuration (Entry _ start end es) =
  let s = timeOfDayToTime start
      e = timeOfDayToTime end
      d = Sum ((fromIntegral . diffTimeToPicoseconds $ e - s) / 1e12 / 60 / 60)
  in (es, d)

-- | Show a float to one decimal.
pf :: Float -> String
pf x = showFFloat (Just 1) x ""

-- | Pretty show a tag value.
prettyShow :: TagVal (Sum Float) -> String
prettyShow (TagVal t (Sum v)) = pf v ++ " " ++ t

-- | Go back in time till we find a Monday.
findLastMonday :: Day -> Day
findLastMonday d =
  if formatTime defaultTimeLocale "%a" d == "Mon"
    then d
    else findLastMonday $ addDays (-1) d

-- | Predicate matching if an entry is in the work week (Mon-Fri) prior to the given day.
entryInWorkWeek :: Day -> Entry -> Bool
entryInWorkWeek today (Entry d _ _ _) =
  let start = findLastMonday today
      end = addDays 4 start
  in d >= start && d <= end
