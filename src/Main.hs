import Data.Ord
import Data.List
import Data.Monoid
import Data.Time.Clock
import Data.Time.LocalTime
import Numeric
import Pikatok.Entry
import Pikatok.Parser
import Pikatok.Tree
import System.Environment

usage :: String
usage = "Usage: pikatok [--today] <input-file>"

calcDuration :: Entry -> ([String], Sum Float)
calcDuration (Entry _ start end es) =
  let s = timeOfDayToTime start
      e = timeOfDayToTime end
      d = Sum ((fromIntegral . diffTimeToPicoseconds $ e - s) / 1e12 / 60 / 60)
  in (es, d)

pf :: Float -> String
pf x = showFFloat (Just 1) x ""

prettyShow :: TagVal (Sum Float) -> String
prettyShow (TagVal t (Sum v)) = pf v ++ " " ++ t

main :: IO ()
main = do
  args <- getArgs
  now <- getZonedTime
  let today = (localDay . zonedTimeToLocalTime) now
  let (flags, other) = partition ((== "--") . take 2) args
  if length other /= 1
  then putStrLn usage
  else do es <- pikatokParseFile (head other)
          let es' = if "--today" `elem` flags
                    then filter (\(Entry d _ _ _) -> d == today) es
                    else es
          let tree = (sortTree (comparing (Down . tval)) . sumBelow) (mappendTree calcDuration es')
          (putStrLn . simpleShowTree prettyShow) tree
