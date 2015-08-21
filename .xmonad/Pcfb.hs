module Pcfb where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.Either (rights)
import Data.Maybe (isNothing)
import Data.Monoid (mconcat)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime ( getCurrentTimeZone
                           , utcToLocalTime
                           , todHour
                           , todMin
                           , localTimeOfDay
                           )
import Text.ParserCombinators.Parsec

type Project = String
type Time = (Int, Int)

data Stretch = Stretch
    { project   :: Project
    , startTime :: Time
    , endTime   :: Maybe Time
    } deriving (Show)

duration :: Time -> Time -> Int
duration  (h, m) (h', m') = (h' - h) * 60 + (m' - m)

durationOf :: Stretch -> IO Int
durationOf s =
    case endTime s of
      Just end -> return . duration (startTime s) $ end
      Nothing  -> return 1

date :: IO (Integer,Int,Int)
date = getCurrentTime >>= return . toGregorian . utctDay

endOrNow :: Stretch -> IO Time
endOrNow s =
    case endTime s of
      Just end -> return end
      Nothing  -> do
          tz <- getCurrentTimeZone
          now <- getCurrentTime
          let when = localTimeOfDay $ utcToLocalTime tz now
          return (todHour when, todMin when)


dateFile :: IO FilePath
dateFile = do
    (year, month, day) <- date
    return $ mconcat
        [ "/home/bootstrap/.tino/var/"
        , show year
        , "-"
        , pad "0" $ show month
        , "-"
        , pad "0" $ show day
        , ".txt"
        ]
  where
      pad c d =
          if length d < 2
             then "0" ++ d
             else d

projectParser :: GenParser Char st String
projectParser = many $ noneOf " "

timeParser :: GenParser Char st Time
timeParser = do
    many $ string " "
    hour   <- read <$> many digit
    string ":"
    minute <- read <$> many digit
    return (hour, minute)

parseStretch :: String -> Either ParseError Stretch
parseStretch input =
    let parseStretch' = do
          proj  <- projectParser
          start <- timeParser
          end   <- optionMaybe timeParser
          return $ Stretch proj start end
     in parse parseStretch' "(unknown)" input

getStretches :: FilePath -> IO [Stretch]
getStretches file = do
    contents <- readFile file
    return . rights . map parseStretch $ lines contents

todayStretches :: IO [Stretch]
todayStretches = dateFile >>= getStretches

isOpen :: [Stretch] -> Bool
isOpen = isNothing . endTime . last

productive :: Time -> [Stretch] -> IO [(Float, Float)]
productive start ss = fmap (reverse . snd)
                   $ foldM go (0, []) ss
    where
        go (acc, result) s = do
            acc' <- (acc +) <$> durationOf s
            let start = perc acc $ startTime s
            end <- perc acc' <$> endOrNow s
            return (acc', [(start, end)] ++ result)
        perc acc when = fromIntegral acc / fromIntegral (duration start when)



