import Data.Monoid
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
data TagVal a = TagVal String a
  deriving Show

type TagTree a = Tree (TagVal a)

type TimeTree = TagTree [Entry]

blankTree :: Monoid m => TagTree m
blankTree = Node (TagVal "" mempty) []

tagTreeInsert :: Monoid a =>  TagTree a -> ([String], a) -> TagTree a
tagTreeInsert (Node tv children) (t:ts, a) =
  let perf = perforate (\(Node (TagVal s _) _) -> s == t) children
      ins = if null ts
            then (\(Node (TagVal t v) cs) -> Node (TagVal t (v <> a)) cs) -- No more tags left, just mappend on the end of the appropriate child.
            else (\t -> tagTreeInsert t (ts, a)) -- There are some more sub-tags, we are going to need to keep inserting.
  in case perf of Just (as, x, bs) -> Node tv $ as ++ ins x : bs
                  Nothing -> Node tv $ ins (Node (TagVal t mempty) []) : children

-- makeTimeTree :: [Entry] -> TimeTree
-- makeTimeTree = mappendTree $ \e@(Entry _ _ _ es) -> (es, [e])

mappendTree :: Monoid m => (a -> ([String], m)) -> [a] -> TagTree m
mappendTree f es = let es' = map f es
                   in foldl tagTreeInsert blankTree es'

usage :: String
usage = "Usage: pikatok <input-file>"

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

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn usage
  else do es <- pikatokParseFile (args !! 0)
          let tree = prettyShow <$> mappendTree calcDuration es
          (putStrLn . drawTree) tree
