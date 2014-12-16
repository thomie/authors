#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Data.List (genericLength)
import Data.Monoid (Sum(..))
import GHC.Exts (groupWith, sortWith)
import System.Process (readProcess)

type Name = String
type Year = Integer

type Commit = (Name, Year)
type Author = (Name, Sum Integer)

groupByFst :: (Ord a) => [(a, b)] -> [(a, [b])]
groupByFst = map (fst . head &&& map snd) . groupWith fst

parse :: String -> [Commit]
parse = map parseCommit . lines
  where
    parseCommit s = (author, parseYear date)
      where
        (author, date) = read s
        parseYear = read . takeWhile (/= '-')

group :: [Commit] -> [(Year, [Author])]
group = map (fmap sortAuthors) . groupByYear . map authorStats . groupByName
  where
    groupByName = groupByFst
    groupByYear = reverse . groupByFst

    authorStats :: (Name, [Year]) -> (Year, Author)
    authorStats (name, years) = (lastYear, (name, commitCount))
      where lastYear = maximum years
            commitCount = genericLength years

    -- Most active contributor first, alphabetical on equal commit count.
    -- > sortAuthors [("C", 5), ("A", 4), ("B", 5)]
    -- [("B",5),("C",5),("A",4)]
    sortAuthors :: [Author] -> [Author]
    sortAuthors = reverse . sortWith snd . reverse . sortWith fst

formatAuthor :: Author -> String
formatAuthor (name, count) =
  " * " ++ name ++ " (" ++ show (getSum count) ++ ")"

format :: (Year, [Author]) -> String
format (year, authors) = "=== " ++ show year ++ " ===\n" ++
                                         (unlines . map formatAuthor) authors

main :: IO ()
main = do
  s <- readProcess "git" ["log", "--no-merges", "--pretty=format:(\"%aN\",\"%ai\")"] []
  mapM_ (putStrLn . format) $ group . parse $ s
