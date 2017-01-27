module Pikatock.Parser
  ( pikatockParseFile
  ) where

import Data.Char
import Data.Time.Calendar
import Data.Time.LocalTime
import Pikatock.Entry
import Text.Parsec

-- | Type for storing the current state while we're parsing our
-- file. We are storing the current context in which we're reading. We
-- either have no context, we know what the day is, or we have an
-- entire previous entry to infer things from.
data ParseState
  = PSNothing
  | PSDay Day
  | PSPrevInfo Day
               TimeOfDay
               [(SourcePos, String)]

-- | Inefficient whitespace trim, ripped straight from Stack Overflow :O
trim :: String -> String
trim =
  let f = reverse . dropWhile isSpace
  in f . f

skipTillEOL :: Parsec String u ()
skipTillEOL = skipMany $ noneOf "\n\r"

comment :: Parsec String u ()
comment = char '#' >> skipTillEOL >> return ()

date :: Parsec String ParseState ()
date = do
  y <- count 4 digit
  _ <- char '-'
  m <- count 2 digit
  _ <- char '-'
  d <- count 2 digit
  let day = fromGregorian (read y) (read m) (read d)
  putState $ PSDay day

time :: Parsec String s TimeOfDay
time = do
  h <- count 2 digit
  m <- count 2 digit
  let tod = makeTimeOfDayValid (read h) (read m) 0
  case tod of
    Nothing -> fail "What sort of time do you call this?"
    Just t -> return t

-- | Either read a time string as in the `time` function, or if we get
-- a bunch of blank spaces, infer the start time from the end time of
-- the previous entry.
timeStart :: Parsec String ParseState TimeOfDay
timeStart =
  let startExtractor (PSPrevInfo _ s _) = return s
      startExtractor _ = fail "Couldn't infer start time."
  in (count 4 (char ' ') >> getState >>= startExtractor) <|> time

-- | Read a bunch of characters that can make up a category between
-- colons.
chunkchars :: Parsec String u String
chunkchars = many1 (noneOf ":\n\r#")

-- | Read a subcategory chunk. This will start with a colon and
-- continues to the next colon / newline / comment.
chunk :: Parsec String u (SourcePos, String)
chunk = do
  pos <- getPosition
  _ <- char ':'
  s <- chunkchars
  return (pos, trim s)

-- | Read the first chunk of a line. This won't start with a colon and
-- may or may not be present. If it is not present we are going to
-- have to infer what should go here from the previous line.
firstChunk :: Parsec String u (Maybe (SourcePos, String))
firstChunk = do
  pos <- getPosition
  cs <- optionMaybe chunkchars
  return ((,) pos . trim <$> cs)

-- | Try to read the chunks on a line and do any necessary inferring.
chunks :: Parsec String ParseState [(SourcePos, String)]
chunks = do
  state <- getState
  c1 <- firstChunk
  cs <- many chunk
  case c1 of
    Nothing ->
      case inferChunks state cs of
        Left s -> fail s
        Right v -> return v
    Just c -> return (c : cs)

inferChunks
  :: ParseState
  -> [(SourcePos, String)] -- ^ List of entries which are missing their parent categories and need to have them inferred.
  -> Either String [(SourcePos, String)] -- ^ Either some error or the inferred parent cats.
inferChunks state es
        -- The column of the first category on this line.
 =
  let p1 = (sourceColumn . fst) (head es)
        -- Infer the parent categories using the categories from the
        -- previous line (pes).
      infer pes =
        let inferred = takeWhile (\(pos, _) -> sourceColumn pos /= p1) pes
        in inferred ++ es
  in case state of
       PSPrevInfo _ _ pes
                        -- Make sure there is an entry in the previous
                        -- line which starts in the same column as the
                        -- first entry of this line.
        ->
         if any (\(pos, _) -> sourceColumn pos == p1) pes
           then Right $ infer pes
           else Left "Could not infer parent categories, check alignment."
       _ -> Left "Could not infer parent categories, entry out of context?"

-- | Read an entry on a single line.
entry :: Parsec String ParseState Entry
entry = do
  state <- getState
  day <-
    case state of
      PSDay d -> return d
      PSPrevInfo d _ _ -> return d
      PSNothing -> fail "Couldn't infer date."
  start <- timeStart
  _ <- char '-'
  end <- time
  skipMany1 $ char ' '
  cs <- chunks
  optional comment
  let e = Entry day start end (map snd cs)
  putState $ PSPrevInfo day end cs
  return e

-- | Read a date and then a set of entries for that date.
entryGroup :: Parsec String ParseState [Entry]
entryGroup = do
  cruft
  date
  _ <- endOfLine
  es <- entry `endBy1` endOfLine
  cruft
  return es

-- | Read off blank lines and comments.
cruft :: Parsec String u ()
cruft = skipMany (skipMany1 endOfLine <|> comment)

pikatockParse :: Parsec String ParseState [Entry]
pikatockParse = concat <$> many1 entryGroup

pikatockParseFile :: String -> IO [Entry]
pikatockParseFile f = do
  contents <- readFile f
  let result = runParser pikatockParse PSNothing f contents
  case result of
    Left e -> fail (show e)
    Right es -> return es
