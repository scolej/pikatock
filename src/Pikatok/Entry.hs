module Pikatok.Entry
  ( Entry (..)
  ) where

import Data.Time.Calendar
import Data.Time.LocalTime

data Entry = Entry { entryDay   :: Day
                   , entryStart :: TimeOfDay
                   , entryEnd   :: TimeOfDay
                   , entryTags  :: [String]
                   }
  deriving Show
