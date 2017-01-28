module Cli.Front
  ( pikatock
  ) where

import Cli.Args
import Data.Monoid
import Data.Ord
import Pikatock.Entry
import Pikatock.Tree
import Pikatock.Util

-- | A report to be presented to the user.
data PikaReport =
  PikaReport String -- ^ A description of this report.
             (TagTree (Sum Float)) -- ^ The collected results for this report.

instance Show PikaReport where
  show (PikaReport descr tree) = descr ++ "\n" ++ simpleShowTree prettyShow tree

treeify :: [Entry] -> TagTree (Sum Float)
treeify es =
  (sortTree (comparing (Down . tval)) . sumBelow) (mappendTree calcDuration es)

-- | Generate reports about a set of entries.
pikatock
  :: [Entry]
  -- ^ The list of entries we're reporting on.
  -> PikaArgs
  -> [PikaReport] -- ^ A report for each group.
pikatock allEntries (PikaArgs _ f g) =
  let r (descr, es) = PikaReport descr (treeify es)
  in map r (g (filter f allEntries))
