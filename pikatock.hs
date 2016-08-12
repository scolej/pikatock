import Text.Parsec
import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Debug.Trace
import Data.Char

data Chunk = WordsChunk String
           | SepChunk Int
           deriving Show
    
data SourceLine = Date Day          -- ^ Line containing a date.
                | Entry             -- ^ Line containing a log entry.
                  (Maybe TimeOfDay) -- ^ The start time.
                  TimeOfDay         -- ^ The end time component.
                  [Chunk]           -- ^ List of chunks that appear on this line.
                  deriving Show

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f
                           
skipTillEOL = skipMany (noneOf "\n\r")
                           
comment :: Parsec String u (Maybe a)
comment = do
  spaces
  _ <- char '#'
  skipTillEOL
  return Nothing

date :: Parsec String u (Maybe SourceLine)
date = do
  y <- count 4 digit
  _ <- char '-'
  m <- count 2 digit
  _ <- char '-'
  d <- count 2 digit
  return $ Just $ Date $ fromGregorian (read y) (read m) (read d)

time :: Parsec String u (Maybe TimeOfDay)
time = (count 4 space >> return Nothing)
       <|>
       (do h <- count 2 digit
           m <- count 2 digit
           return $ Just $ TimeOfDay (read h) (read m) 0)

chunk :: Parsec String u Chunk
chunk = (char ':' >> getPosition >>= (return . SepChunk . sourceColumn))
        <|>
        (many1 (noneOf ":\n\r#") >>= (return . WordsChunk . trim))
        
entry :: Parsec String u (Maybe SourceLine)
entry = do
  start <- time
  _ <- char '-'
  Just end <- time
  skipMany1 space
  chunks <- many1 chunk
  return $ Just $ Entry start end chunks 
       

line :: Parsec String u (Maybe SourceLine)
line = try date <|> try entry <|> try comment
           
parseSource :: Parsec String u [SourceLine]
parseSource = do
  ls <- line `sepEndBy` endOfLine
  return $ catMaybes ls

               
main :: IO ()
main = do
  contents <- readFile "example.time"
  let Right x = parse parseSource "" contents
  mapM_ (putStrLn . show) x 
  return ()
