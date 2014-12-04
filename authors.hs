#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import System.Process (readProcess)
import GHC.Exts (groupWith, sortWith)

type Name = String
type Year = String
type Count = Int

type Commit = (Name, Year)
type Author = (Name, Count)

groupByFst :: (Ord a) => [(a, b)] -> [(a, [b])]
groupByFst = map (fst . head &&& map snd) . groupWith fst

parse :: String -> [Commit]
parse gitlog = map parseCommit $ lines gitlog
  where
    parseCommit s = (author, parseYear date)
      where
        (author, date) = read s
        parseYear = takeWhile $ not . (== '-')

group :: [Commit] -> [(Year, [Author])]
group = map (fmap sortAuthors) . groupByYear . map authorStats . groupByName
  where
    groupByName = groupByFst
    groupByYear = reverse . groupByFst

    authorStats :: (Name, [Year]) -> (Year, Author)
    authorStats (name, years) = (lastYear, (name, commitCount))
      where lastYear = maximum years
            commitCount = length years

    -- Most active contributor first, alphabetical on equal commit count.
    -- > sortAuthors [("C", 5), ("A", 4), ("B", 5)]
    -- [("B",5),("C",5),("A",4)]
    sortAuthors :: [Author] -> [Author]
    sortAuthors = reverse . sortWith snd . reverse . sortWith fst

formatAuthor :: Author -> String
formatAuthor (name, count) = " * " ++ name ++ " (" ++ (show count) ++ ")"

format :: (Year, [Author]) -> String
format (year, authors) = "=== " ++ year ++ " ===\n" ++
                                         (unlines . map formatAuthor) authors

main :: IO ()
main = do
  s <- readProcess "git" ["log", "--pretty=format:(\"%aN\",\"%ai\")"] []
  mapM_ (putStrLn . format) $ group . parse $ s
