import Data.List
import Data.Monoid
import Data.Ord
import Data.Time.Calendar
import Data.Time.LocalTime
import Pikatok.Entry
import Pikatok.Parser
import Pikatok.Tree
import Pikatok.Util
import System.Environment

usage :: String
usage = "Usage: pikatok [--today | --work-week] <input-file...>"

main :: IO ()
main = do
  args <- getArgs
  now <- getZonedTime
  let today = (localDay . zonedTimeToLocalTime) now
  let (flags, other) = partition ((== "--") . take 2) args
  if length other < 1
    then putStrLn usage
    else do
      es <- concat <$> mapM pikatokParseFile other
      let es'
            | "--today" `elem` flags =
              filter (\(Entry d _ _ _) -> d == today) es
            | "--work-week" `elem` flags = filter (entryInWorkWeek today) es
            | otherwise = es
      let tree =
            (sortTree (comparing (Down . tval)) . sumBelow)
              (mappendTree calcDuration es')
      (putStrLn . simpleShowTree prettyShow) tree
