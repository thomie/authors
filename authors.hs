#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Data.List (genericLength, nub)
import Data.Monoid (Sum(..))
import Data.Tuple (swap)
import System.Process (readProcess)

import GHC.Exts (groupWith, sortWith)

type Name = String
type Year = Integer

type Author = (Name, Sum Integer)

groupByFst :: (Ord a) => [(a, b)] -> [(a, [b])]
groupByFst = map (fst . head &&& map snd) . groupWith fst

parse :: String -> [(Name, Year)]
parse = map parseCommit . lines
  where
    parseCommit s = (author, parseYear date)
      where
        (author, date) = read s
        parseYear = read . takeWhile (/= '-')

authorsByYear :: Bool -> [(Name, Year)] -> [(Year, [Author])]
authorsByYear newcomers = map (fmap sortAuthors) . groupByYear . map authorStats . groupByName
  where
    groupByName :: [(Name, Year)] -> [(Name, [Year])]
    groupByName = groupByFst

    authorStats :: (Name, [Year]) -> (Year, Author)
    authorStats (name, years) = (lastYear, (name, commitCount))
      where lastYear = (if newcomers then minimum else maximum) years
            commitCount = genericLength years

    groupByYear :: [(Year, Author)] -> [(Year, [Author])]
    groupByYear = reverse . groupByFst

    -- Most active contributor first, alphabetical on equal commit count.
    -- > sortAuthors [("C", 5), ("A", 4), ("B", 5)]
    -- [("B",5),("C",5),("A",4)]
    sortAuthors :: [Author] -> [Author]
    sortAuthors = reverse . sortWith snd . reverse . sortWith fst

authorsCount :: Bool -> [(Name, Year)] -> [(Year, Sum Integer)]
authorsCount newcomers = map yearStats . groupByYear . (if newcomers then map firstYear . groupByName else map swap)
  where
    groupByName :: [(Name, Year)] -> [(Name, [Year])]
    groupByName = groupByFst

    firstYear :: (Name, [Year]) -> (Year, Name)
    firstYear (name, years) = (minimum years, name)

    groupByYear :: [(Year, Name)] -> [(Year, [Name])]
    groupByYear = groupByFst

    yearStats :: (Year, [Name]) -> (Year, Sum Integer)
    yearStats (year, names) = (year, genericLength . nub $ names)

formatAuthors :: (Year, [Author]) -> String
formatAuthors (year, authors) = "=== " ++ show year ++ " ===\n" ++
                                         (unlines . map formatAuthor) authors
  where
    formatAuthor :: Author -> String
    formatAuthor (name, count) =
        " * " ++ name ++ " (" ++ show (getSum count) ++ ")"

formatYear :: (Year, Sum Integer) -> String
formatYear (year, Sum n) = show year ++ " " ++ show n

misAttributedCommits = [
      ("Brian McKenna", 2015) -- erikd D600, January
    , ("Zejun Wu", 2015) -- simonmar D974, June
    , ("Bernard Desmyter", 2015) -- thomie D971, June
    , ("John Paul Adrian Glaubitz", 2015) -- slyfox #11209
    ]

main :: IO ()
main = do
  s <- readProcess "git" ["log", "--no-merges", "--pretty=format:(\"%aN\",\"%ai\")"] []
  let p = parse s ++ misAttributedCommits

  -- list
  --mapM_ print (parse s)

  putStrLn ""
  putStrLn "Contributors, listed by year of first commit"
  putStrLn ""
  mapM_ (putStrLn . formatAuthors) $ authorsByYear True $ p

  putStrLn ""
  putStrLn "Contributors, listed by year of last commit"
  putStrLn ""
  mapM_ (putStrLn . formatAuthors) $ authorsByYear False $ p

  putStrLn ""
  putStrLn "Number of newcomers per year"
  mapM_ (putStrLn . formatYear) $ authorsCount True $ p

  putStrLn ""
  putStrLn "Number of contributors per year"
  mapM_ (putStrLn . formatYear) $ authorsCount False $ p

