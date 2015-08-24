module Data.Pcfb where

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
                           , hoursToTimeZone
                           )
import Text.ParserCombinators.Parsec

type Project = String
newtype Time = Time (Int, Int)

instance Show Time where
    show (Time (h, m)) = mconcat
        [ pad2 "0" $ show h
        , ":"
        , pad2 "0" $ show m
        ]

data Stretch = Stretch
    { project   :: Project
    , startTime :: Time
    , endTime   :: Maybe Time
    } deriving (Show)

duration :: Time -> Time -> Int
duration  (Time (h, m)) (Time (h', m')) = (h' - h) * 60 + (m' - m)

durationOf :: Stretch -> IO Int
durationOf s =
    case endTime s of
      Just end -> return . duration (startTime s) $ end
      Nothing  -> return 1

date :: IO (Integer,Int,Int)
date = getCurrentTime >>= return . toGregorian . utctDay

now :: IO Time
now = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    let when = localTimeOfDay $ utcToLocalTime tz now
    return $ Time (todHour when, todMin when)


endOrNow :: Stretch -> IO Time
endOrNow s =
    case endTime s of
      Just end -> return end
      Nothing  -> now

pad2 :: String -> String -> String
pad2 c d =
    if length d < 2
       then c ++ d
       else d

dateFile :: IO FilePath
dateFile = do
    (year, month, day) <- date
    return $ mconcat
        [ "/home/bootstrap/.tino/var/"
        , show year
        , "-"
        , pad2 "0" $ show month
        , "-"
        , pad2 "0" $ show day
        , ".txt"
        ]

projectParser :: GenParser Char st String
projectParser = many $ noneOf " "

timeParser :: GenParser Char st Time
timeParser = do
    many $ string " "
    hour   <- read <$> many digit
    string ":"
    minute <- read <$> many digit
    return $ Time (hour, minute)

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

currentProject :: [Stretch] -> Maybe Project
currentProject ss =
    let p = last ss
     in case endTime p of
          Just _  -> Nothing
          Nothing -> Just $ project p

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

writeStart :: String -> Maybe Time -> IO ()
writeStart p Nothing  = fmap Just now >>= writeStart p
writeStart p (Just t) = do
    file <- dateFile
    appendFile file $ p ++ " " ++ show t

writeEnd :: Maybe Time -> IO ()
writeEnd Nothing  = fmap Just now >>= writeEnd
writeEnd (Just t) = do
    file <- dateFile
    appendFile file $ " " ++ show t ++ "\n"

