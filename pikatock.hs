import Data.Foldable
import Data.List
import Data.Monoid
import Data.Ord
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tree
import Numeric
import Parser
import System.Environment

-- | Split a list with a predicate into the elements before the first
-- match, the first match, and the elements after the first match.
perforate :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
perforate f xs = if any f xs
                 then let (as, b:bs) = break f xs
                      in Just (as, b, bs)
                 else Nothing


-- | Some value tagged with a string.
data TagVal a = TagVal { ttag :: String
                       , tval :: a
                       }
  deriving Show

type TagTree a = Tree (TagVal a)

type TimeTree = TagTree [Entry]

blankTree :: Monoid m => TagTree m
blankTree = Node (TagVal "" mempty) []

tagTreeInsert :: Monoid a =>  TagTree a -> ([String], a) -> TagTree a
tagTreeInsert (Node tv children) (t:ts, a) =
  let perf = perforate (\(Node (TagVal s _) _) -> s == t) children
      ins tree = tagTreeInsert tree (ts, a) -- There are some more sub-tags, we are going to need to keep inserting.
  in case perf of Just (as, x, bs) -> Node tv $ as ++ ins x : bs
                  Nothing -> Node tv $ ins (Node (TagVal t mempty) []) : children
-- Run out of tags to insert, just mappend onto the value we are at.
tagTreeInsert (Node (TagVal t v) children) ([], a) = Node (TagVal t (v <> a)) children

mappendTree :: Monoid m => (a -> ([String], m)) -> [a] -> TagTree m
mappendTree f es = let es' = map f es
                   in foldl tagTreeInsert blankTree es'

usage :: String
usage = "Usage: pikatok [--today] <input-file>"

calcDuration :: Entry -> ([String], Sum Float)
calcDuration (Entry _ start end es) =
  let s = timeOfDayToTime start
      e = timeOfDayToTime end
      d = Sum ((fromIntegral . diffTimeToPicoseconds $ e - s) / 1e12 / 60 / 60)
  in (es, d)

pf :: Float -> String
pf x = showFFloat (Just 2) x ""

prettyShow :: TagVal (Sum Float) -> String
prettyShow (TagVal t (Sum v)) = t ++ " " ++ pf v

sumBelow :: Monoid m => TagTree m -> TagTree m
sumBelow tree@(Node tagval cs) =
  let v' = fold (fmap tval tree)
  in Node tagval {tval = v'} (map sumBelow cs)

sortTree :: Ord o => TagTree o -> TagTree o
sortTree (Node tv cs) = Node tv ((sortBy . comparing . fmap Down) f cs')
  where f (Node (TagVal _ v) _) = v
        cs' = map sortTree cs

main :: IO ()
main = do
  args <- getArgs
  now <- getZonedTime
  let today = (localDay . zonedTimeToLocalTime) now
  let (flags, other) = partition (\a -> ((== "--") . take 2) a) args
  if length other /= 1
  then putStrLn usage
  else do es <- pikatokParseFile (other !! 0)
          let es' = if "--today" `elem` flags
                    then filter (\(Entry d _ _ _) -> d == today) es
                    else es
          let tree = prettyShow <$> (sortTree . sumBelow) (mappendTree calcDuration es')
          (putStrLn . drawTree) tree
