import Cli.Front
import Cli.Args
import Data.List
import Data.Time.LocalTime
import Pikatock.Parser
import System.Environment

main :: IO ()
main = do
  today <- (localDay . zonedTimeToLocalTime) <$> getZonedTime
  Right pa <- readArgs today <$> getArgs
  es <- concat <$> mapM pikatockParseFile (argsFiles pa)
  let reports = pikatock es pa
  putStrLn $ intercalate "\n" $ map show reports
