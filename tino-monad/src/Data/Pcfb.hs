module Data.Pcfb ( Time (..)
                 , Stretch
                 , dateFile
                 , todayStretches
                 , productive
                 , activeProject
                 , writeStart
                 , writeEnd
                 , hello
                 ) where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad       (foldM)
import Data.Either         (rights)
import Data.Maybe          (isNothing)
import Data.Monoid         (mconcat)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime ( getCurrentTimeZone
                           , utcToLocalTime
                           , todHour
                           , todMin
                           , localDay
                           , localTimeOfDay
                           , hoursToTimeZone
                           )
import System.Directory (doesFileExist, getHomeDirectory)
import System.Posix.User (getEffectiveUserName)
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
    tz  <- getCurrentTimeZone
    stamp <- localDay . utcToLocalTime tz <$> getCurrentTime
    home <- getHomeDirectory
    me <- getEffectiveUserName
    return $ mconcat
        [ home
        , "/.tino/var/"
        , show stamp
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

hello :: Maybe Time -> IO ()
hello Nothing  = fmap Just now >>= hello
hello (Just t) = do
    today <- dateFile
    exists <- doesFileExist today
    if exists
       then return ()
       else writeFile today ""

todayStretches :: IO [Stretch]
todayStretches = dateFile >>= getStretches

activeProject :: [Stretch] -> Maybe Project
activeProject [] = Nothing
activeProject ss =
    let p = last ss
     in case endTime p of
          Just _  -> Nothing
          Nothing -> Just $ project p

productive :: [Stretch] -> IO [(Float, Float)]
productive ss = fmap (reverse . snd)
              $ foldM go (0, []) ss
    where
        go (acc, result) s = do
            acc' <- (acc +) <$> durationOf s
            end  <- perc acc' <$> endOrNow s
            let start = perc acc $ startTime s
            return (acc', [(start, end)] ++ result)
        perc acc when = fromIntegral acc / fromIntegral (duration' when)
        duration' = duration (Time (0, 0))

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
