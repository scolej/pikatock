import Parser
import System.Environment

usage :: String
usage = "Usage: pikatok <input-file>"

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn usage
  else do es <- pikatokParseFile (args !! 0)
          mapM_ print es
