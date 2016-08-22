module Pikatok.Entry
  ( Entry (..)
  ) where

import Data.Time.Calendar
import Data.Time.LocalTime

data Entry = Entry Day TimeOfDay TimeOfDay [String]
             deriving Show
