module Pikatok.Util.Test (tests) where

import Data.Time.Calendar
import Data.Time.LocalTime
import Debug.Trace
import Pikatok.Entry
import Pikatok.Util
import Test.HUnit

tests :: Test
tests = TestList $ concat [ findLastMondayTest
                          , entryInWorkWeekTest
                          ]

findLastMondayTest = map TestCase
    [ findLastMonday (fromGregorian 2016 11 15) @=? (fromGregorian 2016 11 14)
    , findLastMonday (fromGregorian 2016 11 17) @=? (fromGregorian 2016 11 14)
    ]

entryInWorkWeekTest =
  let entryForDay d = Entry d midnight midnight []
      now = fromGregorian 2016 11 15
  in map TestCase
       [ entryInWorkWeek now (entryForDay $ fromGregorian 2016 11 15)  @=? True
       , entryInWorkWeek now (entryForDay $ fromGregorian 2016 11 14)  @=? True
       , entryInWorkWeek now (entryForDay $ fromGregorian 2016 11 13)  @=? False
       , entryInWorkWeek now (entryForDay $ fromGregorian 2016 11 19)  @=? False
       ]
